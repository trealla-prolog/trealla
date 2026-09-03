#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <linux/gpio.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <time.h>
#include <unistd.h>

#include "prolog.h"
#include "query.h"

// Linux GPIO character-device builtins, so a hosted build offers the same
// gpio_* and delay_ms predicates as the Raspberry Pi 4 freestanding port and
// the same Prolog runs on both. Opt in with `make GPIO=1`, which selects this
// object through PORT_BIFS_OBJECT; every other build links the empty table in
// src/port_bifs_none.c.
//
// The Linux model is not the bare-metal one. A line is an exclusive
// reservation held open by a file descriptor rather than a register to poke,
// so gpio_mode/2 acquires the line and this file holds the descriptor for the
// life of the process. A pin that a kernel driver or another process already
// owns fails with EBUSY, which has no freestanding counterpart. Nothing here
// pokes a register, so it works the same on a Pi 5's RP1 as on a Pi 4.

#define MAX_GPIO_LINES 256
#define GPIO_CONSUMER "trealla"

#define GPIO_DIRECTION_FLAGS \
	(GPIO_V2_LINE_FLAG_INPUT | GPIO_V2_LINE_FLAG_OUTPUT)

#define GPIO_BIAS_FLAGS \
	(GPIO_V2_LINE_FLAG_BIAS_PULL_UP | GPIO_V2_LINE_FLAG_BIAS_PULL_DOWN \
	| GPIO_V2_LINE_FLAG_BIAS_DISABLED)

typedef struct {
	int fd;					// -1 while the line is not held
	uint64_t flags;			// what it was last requested/configured with
} gpio_line;

static gpio_line s_lines[MAX_GPIO_LINES];
static bool s_lines_ready;
static int s_chip_fd = -1;
static char s_chip_name[256];
static char s_chip_label[32];
static unsigned s_chip_lines;

// throw_error() returns true when a catch/3 handler took the ball, so these
// helpers report their own success and hand the engine's answer back through
// *status for the caller to return unchanged. Same rule as
// ports/rpi4/bif_gpio.c.

static bool chip_is_better(const char *label, const char *name,
	const char *best_name, bool best_is_pinctrl, bool *is_pinctrl)
{
	*is_pinctrl = !strncmp(label, "pinctrl-", 8);

	if (!*best_name)
		return true;

	if (*is_pinctrl != best_is_pinctrl)
		return *is_pinctrl;

	// Neither or both are the SoC's own controller: prefer the lower number,
	// which is what a bare "gpiochip0" assumption would have picked.
	return strcmp(name, best_name) < 0;
}

// Chip numbering has moved between Raspberry Pi OS releases, and an expander
// can take a low number, so match the SoC's pin controller by label rather
// than assuming gpiochip0. TREALLA_GPIOCHIP overrides the choice by name.

static bool open_chip(void)
{
	if (s_chip_fd >= 0)
		return true;

	const char *wanted = getenv("TREALLA_GPIOCHIP");
	DIR *dir = opendir("/dev");

	if (!dir)
		return false;

	char best_name[256] = "";
	char best_label[32] = "";
	unsigned best_lines = 0;
	bool best_is_pinctrl = false;
	struct dirent *entry;

	while ((entry = readdir(dir)) != NULL) {
		if (strncmp(entry->d_name, "gpiochip", 8))
			continue;

		char path[280];
		snprintf(path, sizeof(path), "/dev/%s", entry->d_name);
		int fd = open(path, O_RDWR | O_CLOEXEC);

		if (fd < 0)
			continue;

		struct gpiochip_info info;
		memset(&info, 0, sizeof(info));

		if (ioctl(fd, GPIO_GET_CHIPINFO_IOCTL, &info) < 0) {
			close(fd);
			continue;
		}

		close(fd);
		info.label[sizeof(info.label) - 1] = '\0';
		bool is_pinctrl;

		if (wanted) {
			if (strcmp(entry->d_name, wanted))
				continue;

			is_pinctrl = true;
		} else if (!chip_is_better(info.label, entry->d_name, best_name,
			best_is_pinctrl, &is_pinctrl)) {
			continue;
		}

		snprintf(best_name, sizeof(best_name), "%s", entry->d_name);
		snprintf(best_label, sizeof(best_label), "%s", info.label);
		best_lines = info.lines;
		best_is_pinctrl = is_pinctrl;
	}

	closedir(dir);

	if (!*best_name)
		return false;

	char path[280];
	snprintf(path, sizeof(path), "/dev/%s", best_name);
	int fd = open(path, O_RDWR | O_CLOEXEC);

	if (fd < 0)
		return false;

	if (!s_lines_ready) {
		for (unsigned i = 0; i < MAX_GPIO_LINES; i++)
			s_lines[i].fd = -1;

		s_lines_ready = true;
	}

	s_chip_fd = fd;
	s_chip_lines = best_lines;
	snprintf(s_chip_name, sizeof(s_chip_name), "%s", best_name);
	snprintf(s_chip_label, sizeof(s_chip_label), "%s", best_label);
	return true;
}

static bool need_chip(query *q, cell *p, pl_ctx p_ctx, bool *status)
{
	if (open_chip())
		return true;

	*status = throw_error(q, p, p_ctx, "existence_error", "gpio_chip");
	return false;
}

static bool errno_error(query *q, cell *p, pl_ctx p_ctx, bool *status)
{
	if (errno == EBUSY)
		*status = throw_error(q, p, p_ctx, "permission_error",
			"acquire,gpio_line");
	else if ((errno == EACCES) || (errno == EPERM))
		*status = throw_error(q, p, p_ctx, "permission_error",
			"open,gpio_chip");
	else if (errno == EINVAL)
		*status = throw_error(q, p, p_ctx, "domain_error", "gpio_pin");
	else
		*status = throw_error(q, p, p_ctx, "system_error", "gpio_ioctl");

	return false;
}

// Acquire the line, or reconfigure it when it is already held with different
// flags. Direction and bias are one config to the kernel, so a change to
// either has to carry the other along.

static bool hold_line(query *q, cell *p, pl_ctx p_ctx, unsigned pin,
	uint64_t flags, bool *status)
{
	if (s_lines[pin].fd >= 0) {
		if (s_lines[pin].flags == flags)
			return true;

		struct gpio_v2_line_config config;
		memset(&config, 0, sizeof(config));
		config.flags = flags;

		if (ioctl(s_lines[pin].fd, GPIO_V2_LINE_SET_CONFIG_IOCTL, &config) < 0)
			return errno_error(q, p, p_ctx, status);

		s_lines[pin].flags = flags;
		return true;
	}

	struct gpio_v2_line_request request;
	memset(&request, 0, sizeof(request));
	request.offsets[0] = pin;
	request.num_lines = 1;
	request.config.flags = flags;
	snprintf(request.consumer, sizeof(request.consumer), "%s", GPIO_CONSUMER);

	if (ioctl(s_chip_fd, GPIO_V2_GET_LINE_IOCTL, &request) < 0)
		return errno_error(q, p, p_ctx, status);

	s_lines[pin].fd = request.fd;
	s_lines[pin].flags = flags;
	return true;
}

// Range-checked against the chip's own line count once one is open, and
// against the table size before that, so an out-of-range pin is a domain
// error whether or not the machine has GPIO at all.

static bool get_pin(query *q, cell *p, pl_ctx p_ctx, unsigned *pin,
	bool *status)
{
	if (is_bigint(p)) {
		*status = throw_error(q, p, p_ctx, "domain_error",
			"small_integer_range");
		return false;
	}

	pl_int value = get_smallint(p);
	unsigned limit = s_chip_lines && (s_chip_lines < MAX_GPIO_LINES)
		? s_chip_lines : MAX_GPIO_LINES;

	if ((value < 0) || (value >= (pl_int)limit)) {
		*status = throw_error(q, p, p_ctx, "domain_error", "gpio_pin");
		return false;
	}

	*pin = (unsigned)value;
	return true;
}

static bool get_level(query *q, cell *p, pl_ctx p_ctx, unsigned *level,
	bool *status)
{
	if (is_bigint(p)) {
		*status = throw_error(q, p, p_ctx, "domain_error",
			"small_integer_range");
		return false;
	}

	pl_int value = get_smallint(p);

	if ((value != 0) && (value != 1)) {
		*status = throw_error(q, p, p_ctx, "domain_error", "gpio_level");
		return false;
	}

	*level = (unsigned)value;
	return true;
}

// The alt0-alt5 modes the freestanding port accepts are a pin-multiplexer
// setting, which the character device has no concept of: that belongs to
// pinctrl and the device tree. Only input and output carry over.

static bool bif_gpio_mode_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,atom);
	unsigned pin;
	bool status;

	if (!get_pin(q, p1, p1_ctx, &pin, &status))
		return status;

	if (!need_chip(q, p1, p1_ctx, &status))
		return status;

	const char *mode = C_STR(q, p2);
	uint64_t direction;

	if (!strcmp(mode, "input"))
		direction = GPIO_V2_LINE_FLAG_INPUT;
	else if (!strcmp(mode, "output"))
		direction = GPIO_V2_LINE_FLAG_OUTPUT;
	else
		return throw_error(q, p2, p2_ctx, "domain_error", "gpio_mode");

	uint64_t flags = (s_lines[pin].fd >= 0
		? (s_lines[pin].flags & GPIO_BIAS_FLAGS) : 0) | direction;

	if (!hold_line(q, p1, p1_ctx, pin, flags, &status))
		return status;

	return true;
}

static bool bif_gpio_pull_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,atom);
	unsigned pin;
	bool status;

	if (!get_pin(q, p1, p1_ctx, &pin, &status))
		return status;

	if (!need_chip(q, p1, p1_ctx, &status))
		return status;

	const char *pull = C_STR(q, p2);
	uint64_t bias;

	if (!strcmp(pull, "none"))
		bias = GPIO_V2_LINE_FLAG_BIAS_DISABLED;
	else if (!strcmp(pull, "up"))
		bias = GPIO_V2_LINE_FLAG_BIAS_PULL_UP;
	else if (!strcmp(pull, "down"))
		bias = GPIO_V2_LINE_FLAG_BIAS_PULL_DOWN;
	else
		return throw_error(q, p2, p2_ctx, "domain_error", "gpio_pull");

	uint64_t direction = s_lines[pin].fd >= 0
		? (s_lines[pin].flags & GPIO_DIRECTION_FLAGS)
		: GPIO_V2_LINE_FLAG_INPUT;

	if (!hold_line(q, p1, p1_ctx, pin, direction | bias, &status))
		return status;

	return true;
}

// Reading a line the program has not configured acquires it as an input,
// which keeps gpio_read/2 usable on its own the way it is bare metal.

static bool bif_gpio_read_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,any);
	unsigned pin;
	bool status;

	if (!get_pin(q, p1, p1_ctx, &pin, &status))
		return status;

	if (!need_chip(q, p1, p1_ctx, &status))
		return status;

	if (s_lines[pin].fd < 0) {
		if (!hold_line(q, p1, p1_ctx, pin, GPIO_V2_LINE_FLAG_INPUT, &status))
			return status;
	}

	struct gpio_v2_line_values values;
	memset(&values, 0, sizeof(values));
	values.mask = 1;

	if (ioctl(s_lines[pin].fd, GPIO_V2_LINE_GET_VALUES_IOCTL, &values) < 0) {
		errno_error(q, p1, p1_ctx, &status);
		return status;
	}

	cell tmp;
	make_int(&tmp, (values.bits & 1) ? 1 : 0);
	return unify(q, p2, p2_ctx, &tmp, q->st.cur_ctx);
}

// Writing acquires or reconfigures the line as an output. Bare metal would
// silently do nothing when the pin was left an input; making it an output is
// the less surprising reading of a write, and a program that sets the mode
// first - as ports/rpi4/blink.pl does - behaves identically either way.

static bool bif_gpio_write_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);
	unsigned pin, level;
	bool status;

	if (!get_pin(q, p1, p1_ctx, &pin, &status))
		return status;

	if (!need_chip(q, p1, p1_ctx, &status))
		return status;

	if (!get_level(q, p2, p2_ctx, &level, &status))
		return status;

	uint64_t bias = s_lines[pin].fd >= 0
		? (s_lines[pin].flags & GPIO_BIAS_FLAGS) : 0;

	if (!hold_line(q, p1, p1_ctx, pin,
		bias | GPIO_V2_LINE_FLAG_OUTPUT, &status))
		return status;

	struct gpio_v2_line_values values;
	memset(&values, 0, sizeof(values));
	values.mask = 1;
	values.bits = level ? 1 : 0;

	if (ioctl(s_lines[pin].fd, GPIO_V2_LINE_SET_VALUES_IOCTL, &values) < 0) {
		errno_error(q, p1, p1_ctx, &status);
		return status;
	}

	return true;
}

// Hosted has sleep/1 already; delay_ms/1 exists so that a program written for
// the freestanding port, which has neither sleep/1 nor any clock predicate,
// runs here unchanged.

static bool bif_delay_ms_1(query *q)
{
	GET_FIRST_ARG(p1,integer);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error",
			"small_integer_range");

	pl_int requested = get_smallint(p1);

	if (requested < 0)
		return throw_error(q, p1, p1_ctx, "domain_error",
			"not_less_than_zero");

	struct timespec remaining;
	remaining.tv_sec = (time_t)(requested / 1000);
	remaining.tv_nsec = (long)((requested % 1000) * 1000000L);

	while (nanosleep(&remaining, &remaining) < 0) {
		if (errno != EINTR)
			return throw_error(q, p1, p1_ctx, "system_error", "nanosleep");

		CHECK_INTERRUPT();
	}

	return true;
}

// Hosted-only, for working out on the board itself which controller the
// pins actually landed on.

static bool bif_gpio_chip_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);
	bool status;

	if (!need_chip(q, p1, p1_ctx, &status))
		return status;

	cell tmp;
	make_atom(&tmp, new_atom(q->pl, s_chip_name));

	if (!unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx))
		return false;

	make_atom(&tmp, new_atom(q->pl, s_chip_label));

	if (!unify(q, p2, p2_ctx, &tmp, q->st.cur_ctx))
		return false;

	make_int(&tmp, (pl_int)s_chip_lines);
	return unify(q, p3, p3_ctx, &tmp, q->st.cur_ctx);
}

builtins g_port_bifs[] =
{
	{"gpio_mode", 2, bif_gpio_mode_2, "+integer,+atom", false, false, BLAH},
	{"gpio_pull", 2, bif_gpio_pull_2, "+integer,+atom", false, false, BLAH},
	{"gpio_read", 2, bif_gpio_read_2, "+integer,?integer", false, false, BLAH},
	{"gpio_write", 2, bif_gpio_write_2, "+integer,+integer", false, false, BLAH},
	{"delay_ms", 1, bif_delay_ms_1, "+integer", false, false, BLAH},
	{"gpio_chip", 3, bif_gpio_chip_3, "?atom,?atom,?integer", false, false, BLAH},
	{0}
};
