#pragma once

#ifdef __wasi__

#ifndef clock
// older WASI SDK doesn't have a clock emulator
#include <sys/times.h>
#define clock() ( times(NULL) )
#endif

#endif