#include "platform/baremetal/time_hal.h"

#include <semihost.h>
#include <stdint.h>

static uint64_t s_start_elapsed;
static uint64_t s_tick_freq;

static void time_init(void)
{
    s_tick_freq = sys_semihost_tickfreq();
    s_start_elapsed = sys_semihost_elapsed();
}

uint64_t pl4bm_monotonic_ns(void)
{
    if (!s_tick_freq)
        time_init();

    uint64_t elapsed = sys_semihost_elapsed() - s_start_elapsed;
    uint64_t secs = elapsed / s_tick_freq;
    uint64_t sub = elapsed % s_tick_freq;

    return secs * 1000000000ULL + sub * 1000000000ULL / s_tick_freq;
}
