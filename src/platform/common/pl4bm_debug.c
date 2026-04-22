#include "pl4bm_debug.h"

#include <stdio.h>
#include <stdlib.h>

PL4BM_NORETURN void pl4bm_trap_halt(void)
{
#if defined(__arm__) || defined(__thumb__)
    __asm volatile("cpsid i");
    __asm volatile("bkpt #0");
    for (;;) {
        __asm volatile("wfi");
    }
#else
    abort();
    for (;;) {
    }
#endif
}

PL4BM_NORETURN void pl4bm_vpanic_impl(const char *kind, const char *expr, const char *func,
                                      void *caller, const char *fmt, va_list ap)
{
    printf("\n[PL4BM %s]\n", kind ? kind : "PANIC");

    if (func) {
        printf("function: %s\n", func);
    }

    if (caller) {
        printf("caller:   %p\n", caller);
    }

    if (expr) {
        printf("expr:     %s\n", expr);
    }

    if (fmt) {
        printf("details:  ");
        vprintf(fmt, ap);
        printf("\n");
    }

    fflush(stdout);
    fflush(stderr);

    pl4bm_trap_halt();
}

PL4BM_NORETURN void pl4bm_panic_impl(const char *kind, const char *expr, const char *func,
                                     void *caller, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    pl4bm_vpanic_impl(kind, expr, func, caller, fmt, ap);
    va_end(ap);

    for (;;) {
    }
}

void pl4bm_vlog_impl(const char *kind, const char *func, void *caller, const char *fmt, va_list ap)
{
    printf("[PL4BM %s]", kind ? kind : "LOG");

    if (func) {
        printf(" %s", func);
    }

    if (caller) {
        printf(" caller=%p", caller);
    }

    if (fmt) {
        printf(": ");
        vprintf(fmt, ap);
    }

    printf("\n");
}

void pl4bm_log_impl(const char *kind, const char *func, void *caller, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    pl4bm_vlog_impl(kind, func, caller, fmt, ap);
    va_end(ap);
}
