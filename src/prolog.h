#pragma once

#include "internal.h"

builtins *get_builtin(prolog *pl, const char *name, size_t len, unsigned arity, bool *found, bool *evaluable);
builtins *get_help(prolog *pl, const char *name, unsigned arity, bool *found, bool *evaluable);
module *find_module(prolog *pl, const char *name);
pl_idx new_atom(prolog *pl, const char *name);
bool is_multifile_in_db(prolog *pl, const char *mod, const char *name, unsigned arity);
void load_builtins(prolog *pl);
void uuid_gen(prolog *pl, uuid *u);
void keyvalfree(const void *key, const void *val, const void *p);
void ptrfree(const void *key, const void *val, const void *p);
void thread_initialize(prolog *pl);
void thread_cancel_all(prolog *pl);

#define MAX_PIDS 64

extern pl_idx g_empty_s, g_dot_s, g_cut_s, g_nil_s, g_true_s, g_fail_s;
extern pl_idx g_anon_s, g_neck_s, g_eof_s, g_lt_s, g_gt_s, g_eq_s, g_false_s;
extern pl_idx g_sys_elapsed_s, g_sys_queue_s, g_braces_s, g_call_s, g_braces_s;
extern pl_idx g_sys_stream_property_s, g_unify_s, g_on_s, g_off_s, g_sys_var_s;
extern pl_idx g_plus_s, g_minus_s, g_once_s, g_post_unify_hook_s, g_sys_record_key_s;
extern pl_idx g_conjunction_s, g_disjunction_s, g_at_s, g_sys_ne_s, g_sys_incr_s;
extern pl_idx g_dcg_s, g_throw_s, g_sys_block_catcher_s, g_sys_drop_barrier_s;
extern pl_idx g_if_then_s, g_soft_cut_s, g_negation_s;
extern pl_idx g_error_s, g_slash_s, g_sys_cleanup_if_det_s;
extern pl_idx g_goal_expansion_s, g_term_expansion_s, g_tm_s, g_float_s;
extern pl_idx g_sys_cut_if_det_s, g_as_s, g_colon_s, g_member_s;
extern pl_idx g_caret_s, g_sys_counter_s, g_catch_s, g_memberchk_s;

extern void convert_path(char *filename);

extern void sigfn(int s);

extern builtins g_iso_bifs[];
extern builtins g_sregex_bifs[];
extern builtins g_other_bifs[];
extern builtins g_sort_bifs[];
extern builtins g_ffi_bifs[];
extern builtins g_posix_bifs[];
extern builtins g_contrib_bifs[];
extern builtins g_files_bifs[];
extern builtins g_evaluable_bifs[];
extern builtins g_tasks_bifs[];
extern builtins g_maps_bifs[];
extern builtins g_threads_bifs[];

extern void keyfree(const void *key, const void *val, const void *p);

