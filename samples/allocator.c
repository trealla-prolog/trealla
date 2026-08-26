#include <stdio.h>

#include "trealla.h"

int main(void)
{
	if (!pl_set_allocator(NULL))
		return 1;

	prolog *pl = pl_create();

	if (!pl)
		return 1;

	pl_destroy(pl);
	pl_allocator_stats stats;
	pl_get_allocator_stats(&stats);
	if (stats.current_bytes) {
		fprintf(stderr,
			"allocator leak: current=%zu peak=%zu allocations=%zu failures=%zu\n",
			stats.current_bytes, stats.peak_bytes, stats.allocation_count,
			stats.failure_count);
		return 1;
	}

	return 0;
}
