#pragma once

#include <stdarg.h>

#if defined(__GNUC__) || defined(__clang__)
#define PL4BM_NORETURN __attribute__((noreturn))
#define PL4BM_PRINTF_LIKE(fmt_idx, first_arg_idx)                                                  \
    __attribute__((format(printf, fmt_idx, first_arg_idx)))
#else
#define PL4BM_NORETURN
#define PL4BM_PRINTF_LIKE(fmt_idx, first_arg_idx)
#endif

#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L)
#define PL4BM_FUNC __func__
#else
#define PL4BM_FUNC "?"
#endif

#if defined(__GNUC__) || defined(__clang__)
#define PL4BM_CALLER() __builtin_return_address(0)
#else
#define PL4BM_CALLER() ((void *)0)
#endif

#if defined(__arm__) || defined(__thumb__)
#define PL4BM_BREAKPOINT() __asm volatile("bkpt #0")
#elif defined(__aarch64__)
#define PL4BM_BREAKPOINT() __asm volatile("brk #0")
#else
#define PL4BM_BREAKPOINT()
#endif

PL4BM_NORETURN void pl4bm_vpanic_impl(const char *kind, const char *expr, const char *func,
                                      void *caller, const char *fmt, va_list ap);

PL4BM_NORETURN void pl4bm_panic_impl(const char *kind, const char *expr, const char *func,
                                     void *caller, const char *fmt, ...) PL4BM_PRINTF_LIKE(5, 6);

void pl4bm_vlog_impl(const char *kind, const char *func, void *caller, const char *fmt, va_list ap);

void pl4bm_log_impl(const char *kind, const char *func, void *caller, const char *fmt, ...)
    PL4BM_PRINTF_LIKE(4, 5);

PL4BM_NORETURN void pl4bm_trap_halt(void);

#define PL4BM_STUB_PANIC() pl4bm_panic_impl("STUB CALLED", NULL, PL4BM_FUNC, PL4BM_CALLER(), NULL)

#define PL4BM_STUB_PANIC_MSG(fmt, ...)                                                             \
    pl4bm_panic_impl("STUB CALLED", NULL, PL4BM_FUNC, PL4BM_CALLER(), fmt, ##__VA_ARGS__)

#define PL4BM_LOG(fmt, ...) pl4bm_log_impl("LOG", PL4BM_FUNC, PL4BM_CALLER(), fmt, ##__VA_ARGS__)

#define PL4BM_STUB_LOG(fmt, ...)                                                                   \
    pl4bm_log_impl("STUB", PL4BM_FUNC, PL4BM_CALLER(), fmt, ##__VA_ARGS__)

// remove?

#define PL4BM_FATAL(fmt, ...)                                                                      \
    pl4bm_panic_impl("FATAL", NULL, PL4BM_FUNC, PL4BM_CALLER(), fmt, ##__VA_ARGS__)

#define PL4BM_FATAL0() pl4bm_panic_impl("FATAL", NULL, PL4BM_FUNC, PL4BM_CALLER(), NULL)

#define PL4BM_UNIMPLEMENTED()                                                                      \
    pl4bm_panic_impl("UNIMPLEMENTED", NULL, PL4BM_FUNC, PL4BM_CALLER(), NULL)

#define PL4BM_UNIMPLEMENTED_MSG(fmt, ...)                                                          \
    pl4bm_panic_impl("UNIMPLEMENTED", NULL, PL4BM_FUNC, PL4BM_CALLER(), fmt, ##__VA_ARGS__)

#define PL4BM_ASSERT(expr)                                                                         \
    do {                                                                                           \
        if (!(expr)) {                                                                             \
            pl4bm_panic_impl("ASSERT", #expr, PL4BM_FUNC, PL4BM_CALLER(), NULL);                   \
        }                                                                                          \
    } while (0)

#define PL4BM_ASSERT_MSG(expr, fmt, ...)                                                           \
    do {                                                                                           \
        if (!(expr)) {                                                                             \
            pl4bm_panic_impl("ASSERT", #expr, PL4BM_FUNC, PL4BM_CALLER(), fmt, ##__VA_ARGS__);     \
        }                                                                                          \
    } while (0)

#define PL4BM_UNREACHABLE() pl4bm_panic_impl("UNREACHABLE", NULL, PL4BM_FUNC, PL4BM_CALLER(), NULL)

#define PL4BM_UNREACHABLE_MSG(fmt, ...)                                                            \
    pl4bm_panic_impl("UNREACHABLE", NULL, PL4BM_FUNC, PL4BM_CALLER(), fmt, ##__VA_ARGS__)
