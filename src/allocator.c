#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "allocator.h"
#include "trealla.h"

typedef union allocation_header_ {
	struct {
		size_t size;
	} info;
	long double align_long_double;
	void *align_pointer;
	uint64_t align_u64;
} allocation_header;

static void *default_malloc(void *context, size_t size)
{
	(void)context;
	return malloc(size);
}

static void *default_realloc(void *context, void *ptr, size_t size)
{
	(void)context;
	return realloc(ptr, size);
}

static void default_free(void *context, void *ptr)
{
	(void)context;
	free(ptr);
}

static pl_allocator s_allocator = {
	.struct_size = sizeof(pl_allocator),
	.context = NULL,
	.malloc_fn = default_malloc,
	.realloc_fn = default_realloc,
	.free_fn = default_free,
};

static int s_locked;
static size_t s_current_bytes;
static size_t s_peak_bytes;
static size_t s_allocation_count;
static size_t s_failure_count;

static size_t counter_load(const size_t *counter)
{
	return __atomic_load_n(counter, __ATOMIC_RELAXED);
}

static void counter_add(size_t *counter, size_t amount)
{
	__atomic_add_fetch(counter, amount, __ATOMIC_RELAXED);
}

static void counter_sub(size_t *counter, size_t amount)
{
	__atomic_sub_fetch(counter, amount, __ATOMIC_RELAXED);
}

static void update_peak(size_t current)
{
	size_t peak = counter_load(&s_peak_bytes);

	while ((current > peak) && !__atomic_compare_exchange_n(&s_peak_bytes,
		&peak, current, false, __ATOMIC_RELAXED, __ATOMIC_RELAXED))
		;
}

static bool total_size(size_t size, size_t *total)
{
	if (size > (SIZE_MAX - sizeof(allocation_header)))
		return false;

	*total = sizeof(allocation_header) + size;
	return true;
}

bool pl_set_allocator(const pl_allocator *allocator)
{
	if (__atomic_load_n(&s_locked, __ATOMIC_ACQUIRE))
		return false;

	if (!allocator) {
		s_allocator = (pl_allocator){
			.struct_size = sizeof(pl_allocator),
			.context = NULL,
			.malloc_fn = default_malloc,
			.realloc_fn = default_realloc,
			.free_fn = default_free,
		};
		return true;
	}

	if ((allocator->struct_size < sizeof(pl_allocator))
		|| !allocator->malloc_fn || !allocator->realloc_fn || !allocator->free_fn)
		return false;

	s_allocator = *allocator;
	return true;
}

void pl_get_allocator_stats(pl_allocator_stats *stats)
{
	if (!stats)
		return;

	stats->current_bytes = counter_load(&s_current_bytes);
	stats->peak_bytes = counter_load(&s_peak_bytes);
	stats->allocation_count = counter_load(&s_allocation_count);
	stats->failure_count = counter_load(&s_failure_count);
}

void pl_reset_allocator_peak(void)
{
	__atomic_store_n(&s_peak_bytes, counter_load(&s_current_bytes), __ATOMIC_RELAXED);
}

void *tpl_malloc(size_t size)
{
	size_t total;
	__atomic_store_n(&s_locked, 1, __ATOMIC_RELEASE);

	if (!total_size(size, &total)) {
		counter_add(&s_failure_count, 1);
		return NULL;
	}

	allocation_header *header = s_allocator.malloc_fn(s_allocator.context, total);

	if (!header) {
		counter_add(&s_failure_count, 1);
		return NULL;
	}

	header->info.size = size;
	size_t current = __atomic_add_fetch(&s_current_bytes, size, __ATOMIC_RELAXED);
	counter_add(&s_allocation_count, 1);
	update_peak(current);
	return header + 1;
}

void *tpl_calloc(size_t count, size_t size)
{
	if (size && (count > (SIZE_MAX / size))) {
		counter_add(&s_failure_count, 1);
		return NULL;
	}

	size_t bytes = count * size;
	void *ptr = tpl_malloc(bytes);

	if (ptr)
		memset(ptr, 0, bytes);

	return ptr;
}

void *tpl_realloc(void *ptr, size_t size)
{
	if (!ptr)
		return tpl_malloc(size);

	if (!size) {
		tpl_free(ptr);
		return NULL;
	}

	size_t total;

	if (!total_size(size, &total)) {
		counter_add(&s_failure_count, 1);
		return NULL;
	}

	allocation_header *old_header = (allocation_header*)ptr - 1;
	size_t old_size = old_header->info.size;
	allocation_header *header = s_allocator.realloc_fn(s_allocator.context,
		old_header, total);

	if (!header) {
		counter_add(&s_failure_count, 1);
		return NULL;
	}

	header->info.size = size;

	if (size >= old_size)
		counter_add(&s_current_bytes, size - old_size);
	else
		counter_sub(&s_current_bytes, old_size - size);

	counter_add(&s_allocation_count, 1);
	update_peak(counter_load(&s_current_bytes));
	return header + 1;
}

void tpl_free(void *ptr)
{
	if (!ptr)
		return;

	allocation_header *header = (allocation_header*)ptr - 1;
	counter_sub(&s_current_bytes, header->info.size);
	s_allocator.free_fn(s_allocator.context, header);
}

char *tpl_strdup(const char *src)
{
	size_t len = strlen(src) + 1;
	char *dst = tpl_malloc(len);

	if (dst)
		memcpy(dst, src, len);

	return dst;
}

void pl_free(void *ptr)
{
	tpl_free(ptr);
}
