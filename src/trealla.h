#pragma once
#include <stdbool.h>
#include <stdio.h>

#ifdef __wasi__
#define EXPORT(name) __attribute__((export_name(#name)))
#else
#define EXPORT(name)
#endif

typedef struct prolog_ prolog;
typedef struct {} pl_sub_query;

EXPORT(pl_create)
extern prolog *pl_create();
EXPORT(pl_destroy)
extern void pl_destroy(prolog*);

EXPORT(pl_eval)
extern bool pl_eval(prolog*, const char *expr);
EXPORT(pl_consult)
extern bool pl_consult(prolog*, const char *filename);
EXPORT(pl_consult_fp)
extern bool pl_consult_fp(prolog*, FILE *fp, const char *filename);
EXPORT(pl_isatty)
extern bool pl_isatty(prolog*);
EXPORT(pl_stdin)
extern FILE *pl_stdin(prolog*);

EXPORT(pl_query)
extern bool pl_query(prolog*, const char *expr, pl_sub_query **q);
EXPORT(pl_redo)
extern bool pl_redo(pl_sub_query *q);
EXPORT(pl_done)
extern bool pl_done(pl_sub_query *q);	// only call if redo still active
EXPORT(query_did_yield)
extern bool query_did_yield(pl_sub_query *q);

EXPORT(pl_format_string)
size_t pl_format_string(char *dst, size_t dstlen, const char *src, int srclen, bool double_quotes);

int pl_get_stream(prolog*, const char *nane, int len);

EXPORT(get_halt_code)
extern int get_halt_code(prolog*);
EXPORT(get_halt)
extern bool get_halt(prolog*);
EXPORT(get_status)
extern bool get_status(prolog*);
EXPORT(get_redo)
extern bool get_redo(prolog*);
EXPORT(did_dump_vars)
extern bool did_dump_vars(prolog*);

EXPORT(set_trace)
extern void set_trace(prolog*);
EXPORT(set_quiet)
extern void set_quiet(prolog*);
EXPORT(set_noindex)
extern void set_noindex(prolog*);
EXPORT(set_opt)
extern void set_opt(prolog*, int onoff);

extern void convert_path(char *filename);

extern int g_tpl_interrupt, g_ac, g_avc;
extern char **g_av, *g_argv0;
extern char *g_tpl_lib;
extern const char *g_version;

#undef EXPORT
