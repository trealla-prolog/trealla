#pragma once

#ifdef WASI_TARGET_SPIN

#include "spin-http.h"

#define SPIN_HTTP_METHODS_MAX (SPIN_HTTP_METHOD_OPTIONS + 1)
extern const char* SPIN_METHODS[SPIN_HTTP_METHODS_MAX];
extern bool spin_http_method_lookup(const char *name, uint8_t *id);

#endif
