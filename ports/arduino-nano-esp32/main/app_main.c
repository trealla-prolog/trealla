#include <stdint.h>

#include "esp_heap_caps.h"
#include "esp_psram.h"

#include "platform/platform.h"
#include "trealla.h"

extern const unsigned char program_pl_start[] asm("_binary_program_pl_start");
extern const unsigned char program_pl_end[] asm("_binary_program_pl_end");

static void marker(const char *text)
{
	const char *end = text;

	while (*end)
		end++;

	tpl_platform_console_write(TPL_CONSOLE_OUTPUT, text, (size_t)(end - text));
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

static void *psram_malloc(void *context, size_t size)
{
	allocation_control *control = context;

	if (size > control->maximum_request)
		return NULL;

	return heap_caps_malloc(size, MALLOC_CAP_SPIRAM | MALLOC_CAP_8BIT);
}

static void *psram_realloc(void *context, void *ptr, size_t size)
{
	allocation_control *control = context;

	if (size > control->maximum_request)
		return NULL;

	return heap_caps_realloc(ptr, size,
		MALLOC_CAP_SPIRAM | MALLOC_CAP_8BIT);
}

static void psram_free(void *context, void *ptr)
{
	(void)context;
	heap_caps_free(ptr);
}

void app_main(void)
{
	static allocation_control control = {SIZE_MAX};
	pl_allocator allocator = {
		.struct_size = sizeof(allocator),
		.context = &control,
		.malloc_fn = psram_malloc,
		.realloc_fn = psram_realloc,
		.free_fn = psram_free,
	};
	bool solved = esp_psram_is_initialized() && pl_set_allocator(&allocator);

	marker("TREALLA NANO ESP32 BOOT\n");
	marker_size("TREALLA PSRAM FREE ",
		heap_caps_get_free_size(MALLOC_CAP_SPIRAM));

	uint64_t started = tpl_platform_monotonic_usec();
	prolog *pl = solved ? pl_create() : NULL;
	pl_sub_query *q = NULL;

	if (!pl) {
		marker("TREALLA NANO ESP32 FAILED\n");
		tpl_platform_halt(1);
	}

	set_quiet(pl);
	set_dump_vars(pl, 0);
	solved = pl_consult_text(pl, (const char*)program_pl_start,
		(size_t)(program_pl_end - program_pl_start), "nano-esp32-program");

	bool parsed = solved && pl_query(pl, "freestanding_answer(X)", &q, 0);
	pl_term *x = parsed ? pl_binding(q, "X") : NULL;
	int64_t value = 0;
	solved = parsed && get_status(pl) && !get_error(pl)
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

	solved = solved && (tpl_platform_monotonic_usec() >= started);
	pl_destroy(pl);

	pl_allocator_stats final_stats;
	pl_get_allocator_stats(&final_stats);
	marker_size("TREALLA HEAP PEAK ", final_stats.peak_bytes);
	marker_size("TREALLA PSRAM FREE AFTER ",
		heap_caps_get_free_size(MALLOC_CAP_SPIRAM));
	solved = solved && !final_stats.current_bytes;
	marker(solved ? "TREALLA NANO ESP32 COMPLETE\n"
		: "TREALLA NANO ESP32 FAILED\n");
	tpl_platform_halt(solved ? 0 : 1);
}
