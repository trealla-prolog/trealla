#pragma once
#ifdef WASI_IMPORTS
#include <stddef.h>

__attribute__((import_module("trealla"), import_name("host-call")))
extern char *host_call(const char *msg, size_t msg_size, size_t *reply_size);

#endif

