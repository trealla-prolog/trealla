#pragma once
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

typedef struct prolog_ prolog;
typedef struct {} pl_sub_query;

prolog *pl_create();
void pl_destroy(prolog*);

bool pl_consult(prolog*, const char *filename);
bool pl_consult_fp(prolog*, FILE *fp, const char *filename);
bool pl_eval(prolog*, const char *expr, bool interactive);
bool pl_isatty(prolog*);
FILE *pl_stdin(prolog*);
bool pl_restore(prolog*, const char *filename);
bool pl_logging(prolog*, const char *filename);

bool pl_query(prolog*, const char *expr, pl_sub_query **q, unsigned int yield_time_in_ms);
bool pl_yield_at(pl_sub_query *q, unsigned int time_in_ms);
bool pl_did_yield(pl_sub_query *q);
bool pl_redo(pl_sub_query *q);
bool pl_done(pl_sub_query *q);	// only call if redo still active

int get_halt_code(prolog*);
bool get_error(prolog*);
bool get_halt(prolog*);
bool get_status(prolog*);
bool get_redo(prolog*);
bool did_dump_vars(prolog*);

void set_trace(prolog*);
void set_quiet(prolog*);
void set_noindex(prolog*);
void set_opt(prolog*, int onoff);

void convert_path(char *filename);

extern int g_tpl_interrupt;
extern int g_ac, g_avc;
extern char **g_av, *g_argv0;
extern char *g_tpl_lib;
extern const char *g_version;
