#include "prolog.h"
#include "query.h"

// Temporary hosted adapter for the mandatory timing hooks. Phase 3 replaces
// this deterministic counter with the platform service contract.

static uint64_t s_ticks;

uint64_t cpu_time_in_usec(void) { return ++s_ticks; }
uint64_t wall_time_in_usec(void) { return ++s_ticks; }
uint64_t monotonic_time_in_usec(void) { return ++s_ticks; }

bool next_alarm_delay(query *q, unsigned *ms)
{
	(void)q; (void)ms;
	return false;
}

bool has_expired_alarm(query *q)
{
	(void)q;
	return false;
}

char **g_envp;

builtins g_os_bifs[] =
{
	{0}
};
