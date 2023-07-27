#pragma once

#include "internal.h"

module *module_create(prolog *pl, const char *name);
void module_duplicate(prolog *pl, module *m, const char *name, unsigned arity);
void module_destroy(module *m);

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
predicate *search_predicate(module *m, cell *c, bool *prebuilt);
predicate *create_predicate(module *m, cell *c, bool *created);
int index_cmpkey(const void *ptr1, const void *ptr2, const void *param, void *l);
bool needs_quoting(module *m, const char *src, int srclen);
bool unload_file(module *m, const char *filename);
void xref_rule(module *m, clause *t, predicate *parent);
void xref_db(module *m);
const char *get_loaded(const module *m, const char *filename);
builtins *get_module_help(module *m, const char *name, unsigned arity, bool *found, bool *evaluable);
builtins *get_builtin_term(module *m, cell *c, bool *found, bool *evaluable);
void format_property(module *m, char *tmpbuf, size_t buflen, const char *name, unsigned arity, const char *type, bool function);
void format_template(module *m, char *tmpbuf, size_t buflen, const char *name, unsigned arity, const builtins *ptr, bool function, bool alt);
void push_property(module *m, const char *name, unsigned arity, const char *type);
void push_template(module *m, const char *name, unsigned arity, const builtins *ptr);

bool do_use_module_1(module *curr_m, cell *p);
bool do_use_module_2(module *curr_m, cell *p);

#if USE_FFI
bool do_foreign_struct(module *m, cell *p);
bool do_use_foreign_module(module *m, cell *p);
#endif

db_entry *asserta_to_db(module *m, unsigned nbr_vars, unsigned nbr_temporaries, cell *p1, bool consulting);
db_entry *assertz_to_db(module *m, unsigned nbr_vars, unsigned nbr_temporaries, cell *p1, bool consulting);
db_entry *find_in_db(module *m, uuid *ref);
db_entry *erase_from_db(module *m, uuid *ref);

void set_meta_predicate_in_db(module *m, cell *c);
void set_discontiguous_in_db(module *m, const char *name, unsigned arity);
void set_dynamic_in_db(module *m, const char *name, unsigned arity);
void set_multifile_in_db(module *m, const char *name, pl_idx arity);
void set_det_in_db(module *m, const char *name, pl_idx arity);
