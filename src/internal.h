#pragma once

#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <inttypes.h>

#ifndef USE_OPENSSL
#define USE_OPENSSL 0
#endif

#ifndef USE_THREADS
#define USE_THREADS 0
#endif

#if USE_THREADS
#include <pthread.h>
#include <unistd.h>
#endif

typedef double pl_flt;
typedef intmax_t pl_int;
typedef uintmax_t pl_uint;
typedef uint32_t pl_idx;
typedef uint32_t pl_ctx;

#define PL_INT_MAX INTMAX_MAX
#define PL_INT_MIN INTMAX_MIN

#if (__STDC_VERSION__ >= 201112L) && USE_THREADS
#include <stdatomic.h>
#define pl_atomic _Atomic
#else
#define pl_atomic volatile
#endif

#include "list.h"
#include "skiplist.h"
#include "stringbuf.h"
#include "threads.h"
#include "trealla.h"
#include "utf8.h"

#include "imath/imath.h"
#include "imath/imrat.h"

#if defined(_WIN32) || defined(__wasi__)
char *realpath(const char *path, char resolved_path[PATH_MAX]);
#endif

// Sentinel Value
#define ERR_IDX (~(pl_idx)0)
#define IDX_MAX (ERR_IDX-1)
#define CTX_NUL (ERR_IDX-1)

#define MAX_SMALL_STRING ((sizeof(void*)*2)-1)
#define MAX_VAR_POOL_SIZE 16000
#define MAX_ARITY UINT8_MAX
#define MAX_IF_DEPTH 255
#define MAX_VARS 1024
#define MAX_QUEUES 255
#define MAX_MODULES 1024
#define MAX_IGNORES 64000
#define MAX_STREAMS 1024
#define MAX_THREADS 2048
#define MAX_ACTUAL_THREADS 256	// Does nothing

#define STREAM_BUFLEN 1024

#define MAX_OF(a,b) (a) > (b) ? (a) : (b)
#define MIN_OF(a,b) (a) < (b) ? (a) : (b)

// Primary type...

#define is_empty(c) ((c)->tag == TAG_EMPTY)
#define is_var(c) ((c)->tag == TAG_VAR)
#define is_interned(c) ((c)->tag == TAG_INTERNED)
#define is_cstring(c) ((c)->tag == TAG_CSTR)
#define is_integer(c) ((c)->tag == TAG_INT)
#define is_float(c) ((c)->tag == TAG_FLOAT)
#define is_rational(c) ((c)->tag == TAG_RATIONAL)
#define is_indirect(c) ((c)->tag == TAG_INDIRECT)
#define is_dbid(c) ((c)->tag == TAG_DBID)
#define is_kvid(c) ((c)->tag == TAG_KVID)
#define is_blob(c) ((c)->tag == TAG_BLOB)
#define is_end(c) ((c)->tag == TAG_END)

// Derived type...

#define is_iso_atom(c) ((is_interned(c) || is_cstring(c)) && !(c)->arity)
#define is_iso_list(c) (is_interned(c) && ((c)->arity == 2) && ((c)->val_off == g_dot_s))
#define is_smallint(c) (is_integer(c) && !((c)->flags & FLAG_INT_BIG))
#define is_bigint(c) (is_integer(c) && ((c)->flags & FLAG_INT_BIG))
#define is_boolean(c) ((is_interned(c) && !(c)->arity && (((c)->val_off == g_true_s) || ((c)->val_off == g_false_s))))
#define is_atom(c) ((is_interned(c) && !(c)->arity) || is_cstring(c))
#define is_string(c) (is_cstring(c) && ((c)->flags & FLAG_CSTR_STRING))
#define is_codes(c) (is_string(c) && ((c)->flags & FLAG_CSTR_CODES))
#define is_managed(c) ((c)->flags & FLAG_MANAGED)
#define is_cstr_blob(c) (is_cstring(c) && ((c)->flags & FLAG_CSTR_BLOB))
#define is_slice(c) (is_cstr_blob(c) && ((c)->flags & FLAG_CSTR_SLICE))
#define is_strbuf(c) (is_cstr_blob(c) && !((c)->flags & FLAG_CSTR_SLICE))
#define is_list(c) (is_iso_list(c) || is_string(c))
#define is_nil(c) (is_interned(c) && !(c)->arity && ((c)->val_off == g_nil_s))
#define is_anon(c) ((c)->flags & FLAG_VAR_ANON)
#define is_builtin(c) (is_interned(c) && (c)->flags & FLAG_INTERNED_BUILTIN)
#define is_evaluable(c) (is_interned(c) && ((c)->flags & FLAG_INTERNED_EVALUABLE))
#define is_tail_call(c) ((c)->flags & FLAG_INTERNED_TAIL_CALL)
#define is_recursive_call(c) ((c)->flags & FLAG_INTERNED_RECURSIVE_CALL)
#define is_temporary(c) ((c)->flags & FLAG_VAR_TEMPORARY)
#define is_local(c) ((c)->flags & FLAG_VAR_LOCAL)
#define is_void(c) ((c)->flags & FLAG_VAR_VOID)
#define is_global(c) ((c)->flags & FLAG_VAR_GLOBAL)
#define is_attr(c) ((c)->flags & FLAG_VAR_ATTR)
#define is_ground(c) ((c)->flags & FLAG_INTERNED_GROUND)
#define is_ref(c) (is_var(c) && ((c)->flags & FLAG_VAR_REF))
#define is_op(c) ((c)->flags & 0xE000) ? true : false
#define is_callable(c) (is_interned(c) || (is_cstring(c) && !is_string(c)))
#define is_compound(c) (is_interned(c) && (c)->arity)
#define is_structure(c) (is_compound(c) || is_string(c))
#define is_number(c) (is_integer(c) || is_float(c) || is_rational(c))
#define is_atomic(c) (is_atom(c) || is_number(c))
#define is_iso_atomic(c) (is_iso_atom(c) || is_number(c))
#define is_nonvar(c) !is_var(c)

#define is_gt(c,n) (get_smallint(c) > (n))
#define is_ge(c,n) (get_smallint(c) >= (n))
#define is_eq(c,n) (get_smallint(c) == (n))
#define is_ne(c,n) (get_smallint(c) != (n))
#define is_le(c,n) (get_smallint(c) <= (n))
#define is_lt(c,n) (get_smallint(c) < (n))

#define get_list_head(c) ((c) + 1)
#define get_list_tail(c) (get_list_head(c) + get_list_head(c)->num_cells)

#define get_float(c) (c)->val_float
#define set_float(c,v) (c)->val_float = (v)
#define get_smallint(c) (c)->val_int
#define set_smallint(c,v) (c)->val_int = (v)
#define get_smalluint(c) (c)->val_uint
#define set_smalluint(c,v) (c)->val_uint = (v)

#define neg_bigint(c) (c)->val_bigint->ival.sign = MP_NEG
#define neg_smallint(c) (c)->val_int = -llabs((c)->val_int)
#define neg_float(c) (c)->val_float = -fabs((c)->val_float)

#define is_zero(c) (is_bigint(c) ?							\
	mp_int_compare_zero(&(c)->val_bigint->ival) == 0 :		\
	is_smallint(c) ? get_smallint(c) == 0 :					\
	is_float(c) ? get_float(c) == 0.0 : false)

#define is_negative(c) (is_bigint(c) ?						\
	(c)->val_bigint->ival.sign == MP_NEG :					\
	is_smallint(c) ? get_smallint(c) < 0 :					\
	is_float(c) ? get_float(c) < 0.0 : false)

#define is_positive(c) (is_bigint(c) ?						\
	mp_int_compare_zero(&(c)->val_bigint->ival) > 0 :		\
	is_smallint(c) ? get_smallint(c) > 0 :					\
	is_float(c) ? get_float(c) > 0.0 : false)

#define is_not_less_than_zero(c) (is_bigint(c) ?			\
	mp_int_compare_zero(&(c)->val_bigint->ival) >= 0 :		\
	is_smallint(c) ? get_smallint(c) >= 0 :					\
	is_float(c) ? get_float(c) >= 0.0 : false)


extern char *g_global_atoms;

typedef pl_atomic int64_t pl_refcnt;

typedef struct {
	pl_refcnt refcnt;
	size_t len;
	char cstr[];	// 'len+1' bytes
} strbuf;

typedef struct {
	pl_refcnt refcnt;
	union { mpz_t ival; mpq_t irat; };
} bigint;

typedef struct {
	pl_refcnt refcnt;
	char *ptr, *ptr2;
} blob;

#define _CSTRING_STR(c) 										\
	( is_strbuf(c) ? ((c)->val_strb->cstr + (c)->strb_off)		\
	: is_slice(c) ? (c)->val_str								\
	: (char*)(c)->val_chr										\
	)

#define _CSTRING_LEN(c) 										\
	( is_strbuf(c) ? (c)->strb_len								\
	: is_slice(c) ? (c)->str_len								\
	: (c)->chr_len												\
	)

#define _C_STR(pl,c) 											\
	( !is_cstring(c) ? (g_global_atoms + (c)->val_off)			\
	: _CSTRING_STR(c) 											\
	)

#define _C_STRLEN(pl,c) 										\
	( !is_cstring(c) ? strlen(g_global_atoms + (c)->val_off)	\
	: _CSTRING_LEN(c)											\
	)

#define C_STR(x,c) _C_STR((x)->pl, c)
#define C_STRLEN(x,c) _C_STRLEN((x)->pl, c)
#define C_STRLEN_UTF8(c) substrlen_utf8(C_STR(q, c), C_STRLEN(q, c))

#define GET_POOL(x,off) (g_global_atoms + (off))

#define _CMP_SLICE(pl,c,str,len) slicecmp(_C_STR(pl, c), _C_STRLEN(pl, c), str, len)
#define _CMP_SLICE2(pl,c,str) slicecmp2(_C_STR(pl, c), _C_STRLEN(pl, c), str)
#define _CMP_SLICES(pl,c1,c2) slicecmp(_C_STR(pl, c1), _C_STRLEN(pl, c1), _C_STR(pl, c2), _C_STRLEN(pl, c2))
#define _DUP_SLICE(pl,c) slicedup(_C_STR(pl, c), _C_STRLEN(pl, c))

#define CMP_STRING_TO_CSTRN(x,c,str,len) _CMP_SLICE((x)->pl, c, str, len)
#define CMP_STRING_TO_CSTR(x,c,str) _CMP_SLICE2((x)->pl, c, str)
#define CMP_STRING_TO_STRING(x,c1,c2) _CMP_SLICES((x)->pl, c1, c2)
#define DUP_STRING(x,c) _DUP_SLICE((x)->pl, c)

// If changing the order of these: see unify.c dispatch table

enum {
	TAG_EMPTY=0,
	TAG_VAR=1,
	TAG_INTERNED=2,
	TAG_CSTR=3,
	TAG_INT=4,
	TAG_FLOAT=5,
	TAG_RATIONAL=6,
	TAG_INDIRECT=7,
	TAG_BLOB=8,
	TAG_DBID=9,
	TAG_KVID=10,
	TAG_END=11
};

enum {
	FLAG_INT_HANDLE=1<<0,
	FLAG_INT_STREAM=1<<1,
	FLAG_INT_THREAD=1<<2,
	FLAG_INT_MAP=1<<3,
	FLAG_INT_ALIAS=1<<4,
	FLAG_INT_BIG=1<<5,

	FLAG_CSTR_BLOB=1<<0,
	FLAG_CSTR_SLICE=1<<1,
	FLAG_CSTR_STRING=1<<2,				// string of chars
	FLAG_CSTR_CODES=1<<3,				// string of codes

	FLAG_VAR_ANON=1<<0,
	FLAG_VAR_REF=1<<1,
	FLAG_VAR_CYCLIC=1<<2,
	FLAG_VAR_GLOBAL=1<<3,
	FLAG_VAR_TEMPORARY=1<<4,
	FLAG_VAR_LOCAL=1<<5,
	FLAG_VAR_VOID=1<<6,
	FLAG_VAR_ATTR=1<<7,

	FLAG_HANDLE_DLL=1<<0,
	FLAG_HANDLE_FUNC=1<<1,

	FLAG_BLOB_SREGEX=1<<0,

	FLAG_INTERNED_TAIL_CALL=1<<0,
	FLAG_INTERNED_RECURSIVE_CALL=1<<1,
	FLAG_INTERNED_BUILTIN=1<<2,
	FLAG_INTERNED_EVALUABLE=1<<3,
	FLAG_INTERNED_GROUND=1<<4,

	FLAG_MANAGED=1<<12,					// any ref-counted object
	FLAG_END=1<<13						// DO NOT USE
};

// The OP types are stored in the high 3 bits of the flag (13-15)
// and only used during parsing

#define	OP_FX 1
#define	OP_FY 2
#define	OP_XF 3
#define	OP_YF 4
#define	OP_YFX 5
#define	OP_XFX 6
#define	OP_XFY 7

#define IS_PREFIX(op) (((op) == OP_FX) || ((op) == OP_FY))
#define IS_POSTFIX(op) (((op) == OP_XF) || ((op) == OP_YF))
#define IS_INFIX(op) (((op) == OP_XFX) || ((op) == OP_XFY) || ((op) == OP_YFX))
#define IS_XF(op) ((op) == OP_XF)
#define IS_YF(op) ((op) == OP_YF)

#define is_prefix(c) IS_PREFIX(GET_OP(c))
#define is_postfix(c) IS_POSTFIX(GET_OP(c))
#define is_infix(c) IS_INFIX(GET_OP(c))

#define is_fx(c) (GET_OP(c) == OP_FX)
#define is_fy(c) (GET_OP(c) == OP_FY)
#define is_xf(c) (GET_OP(c) == OP_XF)
#define is_yf(c) (GET_OP(c) == OP_YF)
#define is_yfx(c) (GET_OP(c) == OP_YFX)
#define is_xfx(c) (GET_OP(c) == OP_XFX)
#define is_xfy(c) (GET_OP(c) == OP_XFY)

#define SET_OP(c,op) (CLR_OP(c), (c)->flags |= (((uint16_t)(op)) << 13))
#define CLR_OP(c) ((c)->flags &= ~((uint16_t)(0xF) << 13))
#define GET_OP(c) (((c)->flags >> 13) & 0xF)
#define IS_OP(c) (GET_OP(c) != 0)

typedef struct module_ module;
typedef struct query_ query;
typedef struct predicate_ predicate;
typedef struct rule_ rule;
typedef struct cell_ cell;
typedef struct clause_ clause;
typedef struct trail_ trail;
typedef struct frame_ frame;
typedef struct parser_ parser;
typedef struct page_ page;
typedef struct stream_ stream;
typedef struct slot_ slot;
typedef struct choice_ choice;
typedef struct run_state_ run_state;
typedef struct prolog_flags_ prolog_flags;
typedef struct builtins_ builtins;

// Using a fixed-size cell allows having arrays of cells, which is
// basically what a Term is. A compound is a variable length array of
// cells, the length specified by 'num_cells' field in the 1st cell.
// A cell is a tagged union.
// The size should be 24 bytes... (1 + 2) * 8

struct cell_ {

	// 1 * 8 = 8 bytes

	uint8_t tag;
	uint8_t arity;
	uint16_t flags;

	union {
		uint32_t num_cells;				// number of cells
		uint32_t mid;					// used with TAG_EMPTY
	};

	// 2 * 8 = 16 bytes.

	union {

		pl_uint val_uint;
		pl_int val_int;
		pl_flt val_float;
		bigint *val_bigint;
		blob *val_blob;
		uint16_t priority;				// used during parsing

		struct {
			uint8_t	chr_len;
			char val_chr[MAX_SMALL_STRING];
		};

		struct {
			strbuf *val_strb;			// ref-counted string
			uint32_t strb_off;			// ... offset
			uint32_t strb_len;			// ... length
		};

		struct {
			char *val_str;				// static string
			uint64_t str_len;			// ... length
		};

		struct {
			union {
				predicate *match;		// used with TAG_INTERNED
				builtins *bif_ptr;		// used with TAG_INTERNED
				cell *tmp_attrs;		// used with TAG_VAR in copy_term
				cell *val_ptr;			// used with TAG_INDIRECT
				cell *val_attrs;		// used with TAG_EMPTY in slot
			};

			uint32_t var_num;			// used with TAG_VAR

			union {
				uint32_t val_off;		// used with TAG_INTERNED / TAG_VAR -FLAG_VAR_REF
				pl_ctx val_ctx;			// used with TAG_INDIRECT / TAG_VAR +FLAG_VAR_REF
			};
		};

		struct {
			cell *ret_instr;			// used with TAG_EMPTY in call
			uint64_t chgen;				// saves choice generation
		};
	};
};

typedef struct {
	uint64_t u1, u2;					// TODO: proper uuid's
} uuid;

struct clause_ {
	cell *alt;							// alternate representation
	pl_idx cidx, num_allocated_cells;
	unsigned num_vars;
	bool is_first_cut:1;
	bool is_cut_only:1;
	bool is_unique:1;
	bool is_fact:1;
	bool is_deleted:1;
	cell cells[];						// 'num_allocated_cells'
};

struct rule_ {
	lnode hdr;							// must be first
	predicate *owner;
	rule *prev, *next;
	const char *filename;
	uuid u;
	uint64_t db_id, matched, attempted, tcos;
	uint64_t dbgen_created, dbgen_retracted;
	unsigned line_num_start, line_num_end;
	clause cl;
};

// Note: use head/tail as an entry can't be on two intrusive lists

struct predicate_ {
	lnode hdr;							// must be first
	predicate *alias;
	rule *head, *tail;
	module *m;
	skiplist *idx1, *idx2;
	const char *filename;
	cell *meta_args;
	list dirty;
	cell key;
	pl_refcnt refcnt, cnt, db_id;
	unsigned max_vars;
	bool is_reload:1;
	bool is_builtin:1;
	bool is_public:1;
	bool is_dynamic:1;
	bool is_tabled:1;
	bool is_meta_predicate:1;
	bool is_multifile:1;
	bool is_discontiguous:1;
	bool is_abolished:1;
	bool is_noindex:1;
	bool is_check_directive:1;
	bool is_processed:1;
	bool is_var_in_first_arg:1;
	bool is_iso:1;
	bool is_dirty:1;
};

#define BLAH1 false, false, {0}, {0}, 0, NULL, NULL, NULL, NULL, NULL
#define BLAH false, false, {0}, {0}, 0, NULL, NULL, NULL, NULL, NULL, NULL

#define MAX_FFI_ARGS 64

struct builtins_ {
	const char *name;
	unsigned arity;
	bool (*fn)(query*);
	const char *help;
	bool iso;
	bool evaluable;
	bool ffi;
	bool via_directive;
	uint8_t types[MAX_FFI_ARGS];
	const char *names[MAX_FFI_ARGS];
	uint8_t ret_type;
	void *ffi_ret_type;
	const char *ret_name;
	module *m;
	char *desc;
	char *help2;
	char *help_alt;
};

typedef struct {
	const char *name;
	unsigned specifier;
	unsigned priority;
} op_table;

// Where *ctx* is the context of the var
// And *var_num* is the slot within that context

struct trail_ {
	cell *attrs;
	pl_ctx val_ctx;
	uint32_t var_num;
};

// Where *c* is the (possibly) instantiated cell in the current frame
// Where *vgen* & *vgen2* represent the visit generation to check for cyclic terms

struct slot_ {
	cell c;
	uint32_t vgen, vgen2;
};

// Where *prev* is the previous frame
// Where *initial_slots* is the number allocated
// Where *actual_slots* is the number allocated+created
// Where *base* is the offset to first slot in use
// Where *op* is the offset to first overflow slot in use
// Where *chgen* is the choice generation that created this frame

struct frame_ {
	cell *instr;
	module *m;
	uint64_t dbgen, chgen;
	pl_ctx prev;
	pl_idx base, op, hp, heap_num, frame_size;
	unsigned initial_slots, actual_slots, max_vars;
	bool no_recov:1;
};

struct run_state_ {
	predicate *pr;
	cell *instr;
	rule *dbe;
	sliter *iter, *tmp_iter;
	module *m;

	union {
		struct {
			cell *key;
			pl_ctx key_ctx;
			bool karg1_is_ground:1, karg2_is_ground:1, karg3_is_ground:1,
			karg1_is_atomic:1, karg2_is_atomic:1, karg3_is_atomic:1;
		};
		struct { uint64_t uv1, uv2; };
		struct { int64_t v1, v2; };
		int64_t cnt;
	};

	uint64_t timer_started;
	pl_ctx cur_ctx;
	pl_idx new_fp, hp, cp, tp, sp, heap_num;
	uint8_t qnum;
};

struct choice_ {
	run_state st;
	uint64_t gen, chgen, dbgen;
	pl_idx base, op, initial_slots, actual_slots, skip;
	bool catchme_retry:1;
	bool catchme_exception:1;
	bool barrier:1;
	bool register_cleanup:1;
	bool block_catcher:1;
	bool fail_on_retry:1;
	bool succeed_on_retry:1;
	bool no_recov:1;
	bool reset:1;
};

enum { eof_action_eof_code, eof_action_error, eof_action_reset };

struct stream_ {
	union {
		FILE *fp;
		skiplist *keyval;
		query *engine;
		void *handle;
	};

	stringbuf sb_buf;
	char *mode, *filename, *data, *src;
	skiplist *alias;
	void *sslptr;
	parser *p;

	union {
		char srcbuf[STREAM_BUFLEN];
		struct {
			cell *pattern, *curr_yield;
		};
	};

	size_t data_len, alloc_nbytes;
	int ungetch, srclen, chan;
	unsigned rows, cols;
	uint8_t level, eof_action;
	bool ignore:1;
	bool at_end_of_file:1;
	bool bom:1;
	bool repo:1;
	bool binary:1;
	bool did_getc:1;
	bool socket:1;
	bool nodelay:1;
	bool nonblock:1;
	bool udp:1;
	bool ssl:1;
	bool pipe:1;
	bool first_time:1;
	bool is_map:1;
	bool is_memory:1;
	bool is_engine:1;
	bool is_queue:1;
	bool is_mutex:1;
	bool is_alias:1;
};

typedef struct msg_ msg;
typedef struct thread_ thread;

struct thread_ {
	const char *filename;
	prolog *pl;
	query *q;
	skiplist *alias;
	cell *goal, *exit_code, *at_exit, *ball;
	list signals, queue;
#if USE_THREADS
    pthread_t id;
    pthread_cond_t cond;
    pthread_mutex_t mutex;
#endif
	unsigned num_vars, at_exit_num_vars, num_locks;
	int chan, locked_by;
	lock guard;
	pl_atomic bool is_active;
	bool is_init:1;
	bool is_finished:1;
	bool is_detached:1;
	bool is_exception:1;
	bool is_queue_only:1;
	bool is_mutex_only:1;
};

struct page_ {
	page *next;
	union {
		cell *cells;
		frame *frames;
	};
	pl_idx idx, page_size;
	unsigned num;
};

enum q_retry { QUERY_OK=0, QUERY_NOOP=1, QUERY_RETRY=2, QUERY_EXCEPTION=3 };
enum unknowns { UNK_FAIL=0, UNK_ERROR=1, UNK_WARNING=2, UNK_CHANGEABLE=3 };
enum occurs { OCCURS_CHECK_FALSE=0, OCCURS_CHECK_TRUE=1, OCCURS_CHECK_ERROR = 2 };

struct prolog_flags_ {
	enum occurs occurs_check;
	enum unknowns unknown, syntax_error;
	bool double_quote_codes:1;
	bool double_quote_chars:1;
	bool double_quote_atom:1;
	bool character_escapes:1;
	bool char_conversion:1;
	bool strict_iso:1;
	bool debug:1;
	bool json:1;
	bool var_prefix:1;
};

struct query_ {
	lnode hdr;							// must be first
	query *prev, *next, *parent;
	module *current_m;
	prolog *pl;
	parser *top, *p;
	frame *frames;
	slot *slots;
	choice *choices;
	trail *trails;
	cell *tmp_heap, *last_arg, *variable_names, *ball, *cont, *suspect;
	cell *queue[MAX_QUEUES], *tmpq[MAX_QUEUES];
	page *heap_pages;
	slot *save_e;
	query *tasks;
	skiplist *vars;
	void *thread_ptr;
	list dirty;
	cell accum;
	mpz_t tmp_ival;
	mpq_t tmp_irat;
	run_state st;
	stringbuf sb_buf;
	char tmpbuf[256];
	bool ignores[MAX_IGNORES];
	uint64_t total_goals, total_backtracks, total_retries, total_matches, total_inferences;
	uint64_t total_tcos, total_recovs, total_matched, total_no_recovs;
	uint64_t step, qid, tmo_msecs, chgen, cycle_error;
	uint64_t get_started, autofail_n, yield_at;
	uint64_t cpu_started, time_cpu_last_started, future;
	unsigned realloc_frames, realloc_choices, realloc_slots, realloc_trails;
	unsigned max_depth, max_eval_depth, print_idx, tab_idx, dump_var_num;
	unsigned varno, tab0_varno, curr_engine, curr_chan, my_chan;
	unsigned s_cnt, retries, popp;
	pl_ctx latest_ctx, variable_names_ctx, dump_var_ctx, ball_ctx, cont_ctx;
	pl_idx tmphp;
	pl_idx frames_size, slots_size, trails_size, choices_size;
	pl_idx hw_choices, hw_frames, hw_slots, hw_trails, hw_heap_num, hw_deref;
	pl_idx cp, before_hook_tp, qcnt[MAX_QUEUES];
	pl_idx heap_size, tmph_size, total_heaps, total_heapsize;
	pl_idx undo_lo_tp, undo_hi_tp;
	pl_idx q_size[MAX_QUEUES], tmpq_size[MAX_QUEUES], qp[MAX_QUEUES];
	prolog_flags flags;
	enum q_retry retry;
	int is_cyclic1, is_cyclic2;
	uint32_t vgen;
	int8_t halt_code;
	int8_t quoted;
	enum { WAS_OTHER, WAS_SPACE, WAS_COMMA, WAS_SYMBOL } last_thing;
	bool oom:1;
	bool thread_signal:1;
	bool done:1;
	bool noskip:1;
	bool parens:1;
	bool in_attvar_print:1;
	bool lists_ok:1;
	bool fail_on_retry:1;
	bool noretry:1;
	bool is_redo:1;
	bool run_hook:1;
	bool do_dump_vars:1;
	bool is_dump_vars:1;
	bool portray_vars:1;
	bool status:1;
	bool no_recov:1;
	bool has_vars:1;
	bool error:1;
	bool did_throw:1;
	bool trace:1;
	bool creep:1;
	bool eval:1;
	bool yield_after:1;
	bool yielded:1;
	bool is_task:1;
	bool json:1;
	bool nl:1;
	bool fullstop:1;
	bool portrayed:1;
	bool ignore_ops:1;
	bool numbervars:1;
	bool halt:1;
	bool abort:1;
	bool spawned:1;
	bool run_init:1;
	bool varnames:1;
	bool listing:1;
	bool did_quote:1;
	bool is_input:1;
	bool is_engine:1;
	bool ops_dirty:1;
	bool noderef:1;
	bool double_quotes:1;
	bool end_wait:1;
	bool did_unhandled_exception:1;
	bool access_private:1;
};

struct parser_ {
	struct {
		char pool[MAX_VAR_POOL_SIZE];
		unsigned used[MAX_VARS];
		unsigned depth[MAX_VARS];
		unsigned in_body[MAX_VARS];
		unsigned in_head[MAX_VARS];
		pl_idx off[MAX_VARS];
		uint8_t vars[MAX_VARS];
		unsigned num_vars;
	} vartab;

	prolog *pl;
	FILE *fp;
	module *m;
	clause *cl;
	cell v;
	stringbuf token_buf;
	prolog_flags flags;
	query *q;
	char *save_line, *srcptr, *error_desc;
	size_t token_size, n_line, pos_start;
	unsigned line_num, line_num_start;
	unsigned depth, read_term_slots, num_vars;
	unsigned nesting_parens, nesting_braces, nesting_brackets;
	int quote_char, entered;
	int8_t dq_consing;
	bool error, if_depth[MAX_IF_DEPTH];
	bool was_consing:1;
	bool was_string:1;
	bool was_partial:1;
	bool did_getline:1;
	bool already_loaded_error:1;
	bool do_read_term:1;
	bool internal:1;
	bool one_shot:1;
	bool start_term:1;
	bool end_of_term:1;
	bool end_of_file:1;
	bool is_directive:1;
	bool is_command:1;
	bool is_comment:1;
	bool is_consulting:1;
	bool is_symbol:1;
	bool is_string:1;
	bool is_quoted:1;
	bool is_var:1;
	bool is_op:1;
	bool skip:1;
	bool last_close:1;
	bool last_neg:1;
	bool no_fp:1;
	bool reuse:1;
	bool interactive:1;
	bool in_body:1;
	bool is_number_chars:1;
};

typedef struct loaded_file_ loaded_file;

// Predicate indicator...

typedef struct pi_ {
	lnode hdr;							// must be first
	struct pi_ *prev, *next;			// ???
	cell key;
} pi;

struct module_ {
	lnode hdr;							// must be first
	module *used[MAX_MODULES];
	module *orig;
	prolog *pl;
	pi *gex_head, *gex_tail;			// goal expansion ??? (see pi_ above, why not use list?)
	parser *p;
	FILE *fp;
	const char *filename, *name, *actual_filename;
	skiplist *index, *ops, *defops;
	loaded_file *loaded_files;
	lock guard;
	list predicates;
	prolog_flags flags;
	unsigned id, idx_used, arity;
	int if_depth;
	bool ifs_blocked[MAX_IF_DEPTH];
	bool ifs_done[MAX_IF_DEPTH];
	bool user_ops:1;
	bool prebuilt:1;
	bool make_public:1;
	bool loaded_properties:1;
	bool loading:1;
	bool error:1;
	bool ignore_vars:1;
	bool wild_goal_expansion:1;
	bool make:1;
	bool run_init:1;
};

typedef struct {
	pl_ctx ctx;
	pl_idx val_off;
	unsigned var_num, cnt;
	bool is_anon;
} var_item;

struct prolog_ {
	stream streams[MAX_STREAMS];
	thread threads[MAX_THREADS];
	module *modmap[MAX_MODULES];
	struct { pl_idx tab1[MAX_IGNORES], tab2[MAX_IGNORES]; };
	list modules;
	module *system_m, *user_m, *m, *dcgs;
	var_item *tabs;
	parser *p;
	skiplist *biftab, *keyval, *help, *fortab;
	FILE *logfp;
	lock guard;
	size_t tabs_size;
	uint64_t s_last, s_cnt, seed, str_cnt, thr_cnt;
	pl_refcnt q_cnt, dbgen;
	unsigned next_mod_id, def_max_depth, my_chan;
	unsigned current_input, current_output, current_error;
	pl_uint rnd_seed;
	int8_t halt_code, opt;
	bool rnd_first_time:1;
	bool def_quoted:1;
	bool def_double_quotes:1;
	bool is_redo:1;
	bool is_query:1;
	bool halt:1;
	bool status:1;
	bool error:1;
	bool did_dump_vars:1;
	bool autofail:1;
	bool quiet:1;
	bool noindex:1;
	bool iso_only:1;
	bool trace:1;
	bool in_goal_expansion:1;
	bool global_bb:1;
};

extern pl_idx g_empty_s, g_pair_s, g_dot_s, g_cut_s, g_nil_s, g_true_s, g_fail_s;
extern pl_idx g_anon_s, g_neck_s, g_eof_s, g_lt_s, g_false_s, g_once_s;
extern pl_idx g_gt_s, g_eq_s, g_sys_elapsed_s, g_sys_queue_s, g_braces_s;
extern pl_idx g_sys_stream_property_s, g_unify_s, g_on_s, g_off_s, g_sys_var_s;
extern pl_idx g_call_s, g_braces_s, g_plus_s, g_minus_s, g_post_unify_hook_s;
extern bool do_erase(module *m, const char *str);

extern unsigned g_cpu_count;

#define share_cell(c) if (is_managed(c)) share_cell_(c)

inline static void share_cell_(const cell *c)
{
	if (is_strbuf(c))
		c->val_strb->refcnt++;
	else if (is_bigint(c))
		c->val_bigint->refcnt++;
	else if (is_rational(c))
		c->val_bigint->refcnt++;
	else if (is_blob(c))
		c->val_blob->refcnt++;
	else if (is_dbid(c))
		c->val_blob->refcnt++;
	else if (is_kvid(c))
		c->val_blob->refcnt++;
}

#define unshare_cell(c) if (is_managed(c)) unshare_cell_(c)

inline static void unshare_cell_(cell *c)
{
	if (is_strbuf(c)) {
		if (--c->val_strb->refcnt == 0) {
			free(c->val_strb);
			c->tag = TAG_EMPTY;
		}
	} else if (is_bigint(c)) {
		if (--c->val_bigint->refcnt == 0)	{
			mp_int_clear(&c->val_bigint->ival);
			free(c->val_bigint);
			c->tag = TAG_EMPTY;
		}
	} else if (is_rational(c)) {
		if (--c->val_bigint->refcnt == 0)	{
			mp_rat_clear(&c->val_bigint->irat);
			free(c->val_bigint);
			c->flags = 0;
		}
	} else if (is_blob(c)) {
		if (--c->val_blob->refcnt == 0) {
			free(c->val_blob->ptr2);
			free(c->val_blob->ptr);
			free(c->val_blob);
			c->tag = TAG_EMPTY;
		}
	} else if (is_dbid(c)) {
		if (--c->val_blob->refcnt == 0) {
			module *m = (module*)c->val_blob->ptr;
			const char *ref = (char*)c->val_blob->ptr2;
			do_erase(m, ref);
			free(c->val_blob->ptr2);
			free(c->val_blob);
			c->tag = TAG_EMPTY;
		}
	} else if (is_kvid(c)) {
		if (--c->val_blob->refcnt == 0) {
			module *m = (module*)c->val_blob->ptr;
			const char *ref = (char*)c->val_blob->ptr2;
			sl_del(m->pl->keyval, ref);
			free(c->val_blob->ptr2);
			free(c->val_blob);
			c->tag = TAG_EMPTY;
		}
	}
}

inline static pl_idx move_cells(cell *dst, const cell *src, pl_idx num_cells)
{
	memmove(dst, src, sizeof(cell)*(num_cells));
	return num_cells;
}

inline static pl_idx copy_cells(cell *dst, const cell *src, pl_idx num_cells)
{
	memcpy(dst, src, sizeof(cell)*(num_cells));
	return num_cells;
}

inline static pl_idx copy_cells_by_ref(cell *dst, const cell *src, pl_ctx src_ctx, pl_idx num_cells)
{
	for (pl_idx i = 0; i < num_cells; i++, src++, dst++) {
		*dst = *src;

		if (is_var(dst) && !is_ref(dst)) {
			dst->flags |= FLAG_VAR_REF;
			dst->val_ctx = src_ctx;
		}
	}

	return num_cells;
}

inline static pl_idx dup_cells(cell *dst, const cell *src, pl_idx num_cells)
{
	for (pl_idx i = 0; i < num_cells; i++, src++, dst++) {
		*dst = *src;
		share_cell(src);
	}

	return num_cells;
}

inline static pl_idx dup_cells_by_ref(cell *dst, const cell *src, pl_ctx src_ctx, pl_idx num_cells)
{
	for (pl_idx i = 0; i < num_cells; i++, src++, dst++) {
		*dst = *src;
		share_cell(src);

		if (is_var(dst) && !is_ref(dst)) {
			dst->flags |= FLAG_VAR_REF;
			dst->val_ctx = src_ctx;
		}
	}

	return num_cells;
}

#define LIST_HANDLER(l) cell l##_h_tmp, l##_t_tmp
#define LIST_HEAD(l) list_head(l, &l##_h_tmp)
#define LIST_TAIL(l) list_tail(l, &l##_t_tmp)

cell *list_head(cell *l, cell *tmp);
cell *list_tail(cell *l, cell *tmp);

enum clause_type { DO_CLAUSE, DO_RETRACT, DO_RETRACTALL };

char *formatted(const char *src, int srclen, bool dq, bool json);
char *slicedup(const char *s, size_t n);
int slicecmp(const char *s1, size_t len1, const char *s2, size_t len2);
uint64_t get_time_in_usec(void);
uint64_t cpu_time_in_usec(void);
char *relative_to(const char *basefile, const char *relfile);
size_t sprint_int(char *dst, size_t size, pl_int n, int base);
const char *dump_key(const void *k, const void *v, const void *p);

extern unsigned g_max_depth;

#define slicecmp2(s1,l1,s2) slicecmp(s1,l1,s2,strlen(s2))

inline static int fake_strcmp(const void *ptr1, const void *ptr2, const void *param, void *l) {
	return strcmp(ptr1, ptr2);
}

inline static void init_cell(cell *c)
{
	c->tag = TAG_EMPTY;
	c->flags = 0;
	c->num_cells = 0;
	c->arity = 0;
	c->val_attrs = NULL;
}

inline static void predicate_delink(predicate *pr, rule *r)
{
	if (r->prev) r->prev->next = r->next;
	if (r->next) r->next->prev = r->prev;
	if (pr->head == r) pr->head = r->next;
	if (pr->tail == r) pr->tail = r->prev;
}

#define ensure(cond, ...) if (!(cond)) { printf("Error: no memory %s %d\n", __FILE__, __LINE__); abort(); }

