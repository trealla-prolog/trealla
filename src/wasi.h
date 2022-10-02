#pragma once
#ifdef WASI_IMPORTS
#include <stddef.h>
#include <stdbool.h>

// Return statuses for host calls
#define WASM_HOST_CALL_ERROR 0
#define WASM_HOST_CALL_OK    1
#define WASM_HOST_CALL_YIELD 2

__attribute__((import_module("trealla"), import_name("host-call")))
extern int32_t host_call(int32_t subquery, const char *msg, size_t msg_size, char **reply, size_t *reply_size);
__attribute__((import_module("trealla"), import_name("host-resume")))
extern bool host_resume(int32_t subquery, char **reply, size_t *reply_size);

#endif

