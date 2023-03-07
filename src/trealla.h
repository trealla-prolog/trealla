#pragma once
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

typedef struct prolog_ prolog;
typedef struct {} pl_sub_query;

extern prolog *pl_create();
extern void pl_destroy(prolog*);

extern bool pl_consult(prolog*, const char *filename);
extern bool pl_consult_fp(prolog*, FILE *fp, const char *filename);
extern bool pl_eval(prolog*, const char *expr);
extern bool pl_isatty(prolog*);
extern FILE *pl_stdin(prolog*);

extern bool pl_query(prolog*, const char *expr, pl_sub_query **q);
extern bool pl_yield_at(pl_sub_query *q, uint64_t time_in_ms);
extern bool pl_did_yield(pl_sub_query *q);
extern bool pl_redo(pl_sub_query *q);
extern bool pl_done(pl_sub_query *q);	// only call if redo still active

extern int get_halt_code(prolog*);
extern bool get_halt(prolog*);
extern bool get_status(prolog*);
extern bool get_redo(prolog*);
extern bool did_dump_vars(prolog*);

extern void set_trace(prolog*);
extern void set_quiet(prolog*);
extern void set_noindex(prolog*);
extern void set_opt(prolog*, int onoff);

extern void convert_path(char *filename);

extern int g_tpl_interrupt, g_ac, g_avc;
extern char **g_av, *g_argv0;
extern char *g_tpl_lib;
extern const char *g_version;
