#pragma once

#include "internal.h"

module *create_module(prolog *pl, const char *name);
void duplicate_module(prolog *pl, module *m, const char *name, unsigned arity);
void destroy_module(module *m);

bool save_file(module *m, const char *filename);
module *load_file(module *m, const char *filename, bool including);
module *load_fp(module *m, FILE *fp, const char *filename, bool including);
module *load_text(module *m, const char *src, const char *filename);

void retract_from_db(db_entry *dbe);
void convert_to_literal(module *m, cell *c);
unsigned find_op(module *m, const char *name, unsigned specifier);
unsigned search_op(module *m, const char *name, unsigned *specifier, bool hint_prefix);
bool set_op(module *m, const char *name, unsigned specifier, unsigned priority);
predicate *find_functor(module *m, const char *name, unsigned arity);
predicate *find_predicate(module *m, cell *c);
predicate *search_predicate(module *m, cell *c);
predicate *create_predicate(module *m, cell *c);
int index_cmpkey(const void *ptr1, const void *ptr2, const void *param, void *l);
bool needs_quoting(module *m, const char *src, int srclen);
void do_db_load(module *m);
bool unload_file(module *m, const char *filename);
void xref_rule(module *m, clause *t, predicate *parent);
void xref_db(module *m);
void just_in_time_rebuild(predicate *pr);

db_entry *asserta_to_db(module *m, unsigned nbr_vars, unsigned nbr_temporaries, cell *p1, bool consulting);
db_entry *assertz_to_db(module *m, unsigned nbr_vars, unsigned nbr_temporaries, cell *p1, bool consulting);
db_entry *find_in_db(module *m, uuid *ref);
db_entry *erase_from_db(module *m, uuid *ref);

void set_meta_predicate_in_db(module *m, cell *c);
void set_discontiguous_in_db(module *m, const char *name, unsigned arity);
void set_table_in_db(module *m, const char *name, unsigned arity);
void set_dynamic_in_db(module *m, const char *name, unsigned arity);
void set_persist_in_db(module *m, const char *name, unsigned arity);
void set_multifile_in_db(module *m, const char *name, pl_idx_t arity);
void set_det_in_db(module *m, const char *name, pl_idx_t arity);
