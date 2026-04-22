#pragma once

#include <stdint.h>

// Implemented per-board. Returns monotonic nanoseconds since boot.
uint64_t pl4bm_monotonic_ns(void);
