#include "prolog.h"
#include "query.h"
#include "platform/platform.h"

// A freestanding image has no process CPU clock or civil-time clock by
// default. Their callers still need an increasing reference, so all three
// use the mandatory platform monotonic service until those capabilities are
// selected independently.

uint64_t cpu_time_in_usec(void) { return tpl_platform_monotonic_usec(); }
uint64_t wall_time_in_usec(void) { return tpl_platform_monotonic_usec(); }
uint64_t monotonic_time_in_usec(void) { return tpl_platform_monotonic_usec(); }

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
