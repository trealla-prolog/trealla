#include <stdint.h>
#include <stdlib.h>

#include "trealla.h"
#include "platform/platform.h"

extern unsigned char program_pl[];
extern unsigned int program_pl_len;

static void marker(const char *s)
{
	const char *end = s;

	while (*end)
		end++;

	tpl_platform_console_write(TPL_CONSOLE_OUTPUT, s, (size_t)(end - s));
}

static void marker_size(const char *prefix, size_t value)
{
	char digits[3 * sizeof(size_t) + 1];
	size_t used = 0;

	do {
		digits[used++] = (char)('0' + (value % 10));
		value /= 10;
	} while (value);

	marker(prefix);

	while (used)
		tpl_platform_console_write(TPL_CONSOLE_OUTPUT, &digits[--used], 1);

	marker("\n");
}

typedef struct allocation_control_ {
	size_t maximum_request;
} allocation_control;

static void *controlled_malloc(void *context, size_t size)
{
	allocation_control *control = context;
	return size <= control->maximum_request ? malloc(size) : NULL;
}

static void *controlled_realloc(void *context, void *ptr, size_t size)
{
	allocation_control *control = context;
	return size <= control->maximum_request ? realloc(ptr, size) : NULL;
}

static void controlled_free(void *context, void *ptr)
{
	(void)context;
	free(ptr);
}

int main(void)
{
	allocation_control control = {SIZE_MAX};
	pl_allocator allocator = {
		.struct_size = sizeof(allocator),
		.context = &control,
		.malloc_fn = controlled_malloc,
		.realloc_fn = controlled_realloc,
		.free_fn = controlled_free,
	};

	if (!pl_set_allocator(&allocator))
		return 1;

	marker("TREALLA FREESTANDING BOOT\n");
	uint64_t started = tpl_platform_monotonic_usec();
	prolog *pl = pl_create();
	pl_sub_query *q = NULL;

	if (!pl)
		return 1;

	set_quiet(pl);
	set_dump_vars(pl, 0);

	if (!pl_consult_text(pl, (const char*)program_pl, program_pl_len, "freestanding-program")) {
		pl_destroy(pl);
		return 1;
	}

	bool parsed = pl_query(pl, "freestanding_answer(X)", &q, 0);
	pl_term *x = parsed ? pl_binding(q, "X") : NULL;
	int64_t value = 0;
	bool solved = parsed && get_status(pl) && !get_error(pl)
		&& x && pl_get_int64(x, &value) && (value == 42);

	if (q)
		pl_done(q);

	pl_eval(pl, "freestanding_failure", false);
	solved = solved && !get_status(pl) && !get_error(pl);

	pl_eval(pl, "freestanding_platform_probe", false);
	solved = solved && get_status(pl) && !get_error(pl);

	pl_allocator_stats before_failure, after_failure;
	pl_get_allocator_stats(&before_failure);
	control.maximum_request = 128 * 1024;
	pl_eval(pl, "freestanding_oom_probe", false);
	control.maximum_request = SIZE_MAX;
	pl_get_allocator_stats(&after_failure);
	solved = solved && get_status(pl) && !get_error(pl)
		&& (after_failure.failure_count > before_failure.failure_count);

	if (solved)
		marker("TREALLA ALLOCATION FAILURE CONTROLLED\n");

	const char invalid_source[] = "valid_fact.\0hidden_fact.";
	solved = solved && !pl_consult_text(pl, invalid_source,
		sizeof(invalid_source) - 1, "embedded-nul");

	pl_eval(pl, "\\+current_predicate('$server'/3),\\+current_predicate(shell/1),"
		"\\+current_predicate(open/4),\\+current_predicate(directory_files/2)", false);
	solved = solved && get_status(pl) && !get_error(pl);
	solved = solved && (tpl_platform_monotonic_usec() >= started);

	pl_destroy(pl);
	pl_allocator_stats final_stats;
	pl_get_allocator_stats(&final_stats);
	marker_size("TREALLA HEAP PEAK ", final_stats.peak_bytes);
	solved = solved && !final_stats.current_bytes;
	marker(solved ? "TREALLA FREESTANDING COMPLETE\n" : "TREALLA FREESTANDING FAILED\n");
	tpl_platform_halt(solved ? 0 : 1);
}
