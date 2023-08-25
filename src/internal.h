#pragma once

#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <inttypes.h>
#include <unistd.h>
#include "utf8.h"

#ifndef USE_OPENSSL
#define USE_OPENSSL 0
#endif

#ifndef USE_THREADS
#define USE_THREADS 0
#endif

typedef double pl_flt;
typedef intmax_t pl_int;
typedef uintmax_t pl_uint;
typedef uint32_t pl_idx;

#define PL_INT_MAX INTMAX_MAX
#define PL_INT_MIN INTMAX_MIN

#if (__STDC_VERSION__ >= 201112L) && USE_THREADS
#include <stdatomic.h>
#define atomic_t _Atomic
#else
#define atomic_t volatile
#endif

#ifdef _WIN32
#define PATH_SEP_CHAR '\\'
#define NEWLINE_MODE "dos"
#else
#define PATH_SEP_CHAR '/'
#define NEWLINE_MODE "posix"
#endif

#include "skiplist.h"
#include "trealla.h"
#include "cdebug.h"
#include "stringbuf.h"
#include "imath/imath.h"
#include "imath/imrat.h"
#include "sre/re.h"

#if defined(_WIN32) || defined(__wasi__)
char *realpath(const char *path, char resolved_path[PATH_MAX]);
#endif

extern unsigned g_string_cnt, g_interned_cnt;

// Sentinel Value
#define ERR_IDX (~(pl_idx)0)
#define IDX_MAX (ERR_IDX-1)

#define MAX_SMALL_STRING ((sizeof(void*)*2)-1)
#define MAX_VAR_POOL_SIZE 16000
#define MAX_ARITY UINT8_MAX
#define MAX_IF_DEPTH 255
#define MAX_VARS 1024
#define MAX_QUEUES 255
#define MAX_STREAMS 1024
#define MAX_MODULES 1024
#define MAX_IGNORES 64000

#define STREAM_BUFLEN 1024

#define MAX_OF(a,b) (a) > (b) ? (a) : (b)
#define MIN_OF(a,b) (a) < (b) ? (a) : (b)

#define GET_CHOICE(i) (q->choices+(i))
#define GET_CURR_CHOICE() GET_CHOICE(q->cp-1)
#define GET_PREV_CHOICE() GET_CHOICE(q->cp-2)

#define GET_FRAME(i) (q->frames+(i))
#define GET_FIRST_FRAME() GET_FRAME(0)
#define GET_CURR_FRAME() GET_FRAME(q->st.curr_frame)
#define GET_NEW_FRAME() GET_FRAME(q->st.fp)

#define FIRST_ARG(c) ((c)+1)
#define NEXT_ARG(c) ((c)+(c)->nbr_cells)

#define GET_SLOT(f,i) ((i) < (f)->initial_slots ? 			\
	(q->slots+(f)->base+(i)) : 								\
	(q->slots+(f)->overflow+((i)-(f)->initial_slots)) 		\
	)

// Primary type...

#define is_empty(c) ((c)->tag == TAG_EMPTY)
#define is_var(c) ((c)->tag == TAG_VAR)
#define is_interned(c) ((c)->tag == TAG_INTERNED)
#define is_cstring(c) ((c)->tag == TAG_CSTR)
#define is_basic_integer(c) ((c)->tag == TAG_INTEGER)
#define is_integer(c) (((c)->tag == TAG_INTEGER) && !((c)->flags & FLAG_INT_STREAM))
#define is_float(c) ((c)->tag == TAG_DOUBLE)
#define is_rational(c) ((c)->tag == TAG_RATIONAL)
#define is_indirect(c) ((c)->tag == TAG_PTR)
#define is_blob(c) ((c)->tag == TAG_BLOB)
#define is_end(c) ((c)->tag == TAG_END)

// Derived type...

#define is_iso_atom(c) ((is_interned(c) || is_cstring(c)) && !(c)->arity)
#define is_iso_list(c) (is_interned(c) && ((c)->arity == 2) && ((c)->val_off == g_dot_s))

#define get_list_head(c) ((c) + 1)
#define get_list_tail(c) (get_list_head(c) + get_list_head(c)->nbr_cells)

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

#define is_gt(c,n) (get_smallint(c) > (n))
#define is_ge(c,n) (get_smallint(c) >= (n))
#define is_eq(c,n) (get_smallint(c) == (n))
#define is_ne(c,n) (get_smallint(c) != (n))
#define is_le(c,n) (get_smallint(c) <= (n))
#define is_lt(c,n) (get_smallint(c) < (n))

#define is_smallint(c) (is_integer(c) && !((c)->flags & FLAG_MANAGED))
#define is_bigint(c) (is_integer(c) && ((c)->flags & FLAG_MANAGED))
#define is_boolean(c) ((is_interned(c) && !(c)->arity && ((c->val_off == g_true_s) || (c->val_off == g_false_s))))
#define is_atom(c) ((is_interned(c) && !(c)->arity) || is_cstring(c))
#define is_string(c) (is_cstring(c) && ((c)->flags & FLAG_CSTR_STRING))
#define is_managed(c) ((c)->flags & FLAG_MANAGED)
#define is_cstr_blob(c) (is_cstring(c) && ((c)->flags & FLAG_CSTR_BLOB))
#define is_slice(c) (is_cstr_blob(c) && ((c)->flags & FLAG_CSTR_SLICE))
#define is_strbuf(c) (is_cstr_blob(c) && !((c)->flags & FLAG_CSTR_SLICE))
#define is_list(c) (is_iso_list(c) || is_string(c))
#define is_nil(c) (is_interned(c) && !(c)->arity && ((c)->val_off == g_nil_s))
#define is_quoted(c) ((c)->flags & FLAG_CSTR_QUOTED)
#define is_fresh(c) ((c)->flags & FLAG_VAR_FRESH)
#define is_anon(c) ((c)->flags & FLAG_VAR_ANON)
#define is_builtin(c) ((c)->flags & FLAG_BUILTIN)
#define is_evaluable(c) ((c)->flags & FLAG_EVALUABLE)
#define is_tail_recursive(c) ((c)->flags & FLAG_TAIL_REC)
#define is_temporary(c) (is_var(c) && ((c)->flags & FLAG_VAR_TEMPORARY))
#define is_ref(c) (is_var(c) && ((c)->flags & FLAG_VAR_REF))
#define is_op(c) (c->flags & 0xE000) ? true : false
#define is_callable(c) (is_interned(c) || (is_cstring(c) && !is_string(c)))
#define is_structure(c) (is_interned(c) && (c)->arity)
#define is_compound(c) (is_structure(c) || is_string(c))
#define is_number(c) (is_integer(c) || is_float(c) || is_rational(c))
#define is_atomic(c) (is_atom(c) || is_number(c))
#define is_iso_atomic(c) (is_iso_atom(c) || is_number(c))
#define is_nonvar(c) !is_var(c)

typedef struct {
	int64_t refcnt;
	size_t len;
	char cstr[];
} strbuf;

typedef struct {
	int64_t refcnt;
	union { mpz_t ival; mpq_t irat; };
} bigint;

typedef struct {
	int64_t refcnt;
	char *ptr, *ptr2;
} blob;

#define SET_STR(c,s,n,off) {									\
	strbuf *strb = malloc(sizeof(strbuf) + (n) + 1);			\
	check_error(strb);											\
	memcpy(strb->cstr, s, n); 									\
	strb->cstr[n] = 0;											\
	strb->len = n;												\
	strb->refcnt = 1;											\
	g_string_cnt++;												\
	(c)->val_strb = strb;										\
	(c)->strb_off = off;										\
	(c)->strb_len = n;											\
	(c)->flags |= FLAG_MANAGED | FLAG_CSTR_BLOB;				\
	}

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
	( !is_cstring(c) ? ((pl)->pool + (c)->val_off)				\
	: _CSTRING_STR(c) 											\
	)

#define _C_STRLEN(pl,c) 										\
	( !is_cstring(c) ? strlen((pl)->pool + (c)->val_off)		\
	: _CSTRING_LEN(c)											\
	)

#define C_STR(x,c) _C_STR((x)->pl, c)
#define C_STRLEN(x,c) _C_STRLEN((x)->pl, c)
#define C_STRLEN_UTF8(c) substrlen_utf8(C_STR(q, c), C_STRLEN(q, c))

#define GET_POOL(x,off) ((x)->pl->pool + (off))

#define _CMP_SLICE(pl,c,str,len) slicecmp(_C_STR(pl, c), _C_STRLEN(pl, c), str, len)
#define _CMP_SLICE2(pl,c,str) slicecmp2(_C_STR(pl, c), _C_STRLEN(pl, c), str)
#define _CMP_SLICES(pl,c1,c2) slicecmp(_C_STR(pl, c1), _C_STRLEN(pl, c1), _C_STR(pl, c2), _C_STRLEN(pl, c2))
#define _DUP_SLICE(pl,c) slicedup(_C_STR(pl, c), _C_STRLEN(pl, c))

#define CMP_STR_TO_CSTRN(x,c,str,len) _CMP_SLICE((x)->pl, c, str, len)
#define CMP_STR_TO_CSTR(x,c,str) _CMP_SLICE2((x)->pl, c, str)
#define CMP_STR_TO_STR(x,c1,c2) _CMP_SLICES((x)->pl, c1, c2)
#define DUP_STR(x,c) _DUP_SLICE((x)->pl, c)

// If changing the order of these: see unify.c dispatch table

enum {
	TAG_EMPTY=0,
	TAG_VAR=1,
	TAG_INTERNED=2,
	TAG_CSTR=3,
	TAG_INTEGER=4,
	TAG_DOUBLE=5,
	TAG_RATIONAL=6,
	TAG_PTR=7,
	TAG_BLOB=8,
	TAG_END=9
};

enum {
	FLAG_INT_HEX=1<<0,					// used with TAG_INTEGER
	FLAG_INT_OCTAL=1<<1,				// used with TAG_INTEGER
	FLAG_INT_BINARY=1<<2,				// used with TAG_INTEGER
	FLAG_INT_HANDLE=1<<3,				// used with TAG_INTEGER
	FLAG_INT_STREAM=1<<4,				// used with TAG_INTEGER

	FLAG_CSTR_BLOB=1<<0,				// used with TAG_CSTR
	FLAG_CSTR_STRING=1<<1,				// used with TAG_CSTR
	FLAG_CSTR_QUOTED=1<<2,				// used with TAG_CSTR
	FLAG_CSTR_SLICE=1<<3,				// used with TAG_CSTR

	FLAG_VAR_ANON=1<<0,					// used with TAG_VAR
	FLAG_VAR_FRESH=1<<1,				// used with TAG_VAR
	FLAG_VAR_TEMPORARY=1<<2,			// used with TAG_VAR
	FLAG_VAR_REF=1<<3,					// used with TAG_VAR

	FLAG_HANDLE_DLL=1<<0,				// used with FLAG_INT_HANDLE
	FLAG_HANDLE_FUNC=1<<1,				// used with FLAG_INT_HANDLE

	FLAG_BLOB_SRE=1<<0,					// used with TAG_BLOB

	FLAG_FFI=1<<8,
	FLAG_BUILTIN=1<<9,
	FLAG_MANAGED=1<<10,					// any ref-counted object
	FLAG_TAIL_REC=1<<11,
	FLAG_EVALUABLE=1<<12,

	FLAG_END=1<<13
};

// The OP types are stored in the high 3 bits of the flag (13-15)

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
#define IS_OP(c) (GET_OP(c) != 0 ? true : false)

typedef struct module_ module;
typedef struct query_ query;
typedef struct predicate_ predicate;
typedef struct db_entry_ db_entry;
typedef struct cell_ cell;
typedef struct clause_ clause;
typedef struct trail_ trail;
typedef struct frame_ frame;
typedef struct parser_ parser;
typedef struct page_ page;
typedef struct stream_ stream;
typedef struct slot_ slot;
typedef struct choice_ choice;
typedef struct prolog_state_ prolog_state;
typedef struct prolog_flags_ prolog_flags;
typedef struct builtins_ builtins;

// Using a fixed-size cell allows having arrays of cells, which is
// basically what a Term is. A compound is a variable length array of
// cells, the length specified by 'nbr_cells' field in the 1st cell.
// A cell is a tagged union.
// The size should be 24 bytes... (1 + 2) * 8

struct cell_ {

	// 1 * 8 = 8 bytes

	uint8_t tag;
	uint8_t arity;
	uint16_t flags;

	union {
		uint32_t nbr_cells;
		uint16_t mid;				// used with TAG_EMPTY so not counted
	};

	// 2 * 8 = 16 bytes.

	union {

		// Proper types...

		pl_uint val_uint;
		pl_int val_int;
		pl_flt val_float;
		bigint *val_bigint;
		blob *val_blob;
		uint16_t priority;				// used in parsing operators

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
				cell *val_ptr;
				predicate *match;
				builtins *fn_ptr;
				cell *tmp_attrs;		// used with TAG_VAR in copy_term
			};

			uint32_t var_nbr;			// used with TAG_VAR

			union {
				uint32_t val_off;		// used with TAG_VAR & TAG_INTERNED
				pl_idx var_ctx;		// used with TAG_VAR & FLAG_VAR_REF
			};
		};

		struct {
			cell *attrs;				// used with TAG_EMPTY in slot
			pl_idx attrs_ctx;			// to set attributes on a var
		};

		struct {
			cell *val_ret;				// used with TAG_EMPTY saves
			uint64_t cgen;				// choice generation on call
		};
	};
};

typedef struct {
	uint64_t u1, u2;					// TODO: proper uuid's
} uuid;

struct clause_ {
	uint64_t dgen_created, dgen_erased;
	pl_idx allocated_cells, cidx;
	uint32_t nbr_vars;
	uint16_t nbr_temporaries;
	bool is_first_cut:1;
	bool is_cut_only:1;
	bool is_unique:1;
	bool is_fact:1;
	bool is_complex:1;
	bool is_deleted:1;
	cell cells[];
};

struct db_entry_ {
	predicate *owner;
	db_entry *prev, *next;
	const char *filename;
	db_entry *dirty;
	uuid u;
	uint64_t db_id;
	unsigned line_nbr_start, line_nbr_end;
	clause cl;
};

struct predicate_ {
	cell key;
	predicate *prev, *next, *alias;
	db_entry *head, *tail;
	module *m;
	skiplist *idx, *idx2;
	db_entry *dirty_list;
	const char *filename;
	cell *meta_args;
	uint64_t cnt, refcnt, db_id;
	bool is_reload:1;
	bool is_prebuilt:1;
	bool is_public:1;
	bool is_dynamic:1;
	bool is_meta_predicate:1;
	bool is_multifile:1;
	bool is_discontiguous:1;
	bool is_abolished:1;
	bool is_noindex:1;
	bool is_check_directive:1;
	bool is_processed:1;
	bool is_var_in_first_arg:1;
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
// And *var_nbr* is the slot within that context

struct trail_ {
	cell *attrs;
	pl_idx var_ctx, attrs_ctx;
	uint32_t var_nbr;
};

struct slot_ {
	cell c;
	uint32_t vgen, vgen2;	// visit generation
};

// Where 'prev_offset' is the number of frames back
// Where *initial_slots* is the initial number allocated
// Where *actual_slots* is the actual number in use (some maybe created)
// Where *base* is the offset to first slot in use
// Where *overflow* is where new slots are allocated (actual_slots > initial_slots)

struct frame_ {
	cell *curr_cell;
	uint64_t ugen, cgen;
	pl_idx prev_offset, hp, base, overflow, initial_slots, actual_slots;
	uint16_t mid;
};

struct prolog_state_ {
	cell *curr_cell;
	predicate *pr;
	db_entry *dbe;
	sliter *iter, *f_iter;
	module *m, *prev_m;

	union {
		struct { cell *key; bool karg1_is_ground:1; };
		struct { uint64_t v1, v2; };
		int64_t cnt;
	};

	uint64_t timer_started;
	pl_idx curr_frame, fp, hp, tp, sp, pp, key_ctx;
	float prob;
	uint8_t qnbr;
};

struct choice_ {
	prolog_state st;
	uint64_t cgen, frame_cgen, ugen;
	pl_idx base, overflow, initial_slots, actual_slots;
	bool catchme_retry:1;
	bool catchme_exception:1;
	bool barrier:1;
	bool register_cleanup:1;
	bool block_catcher:1;
	bool catcher:1;
	bool fail_on_retry:1;
	bool succeed_on_retry:1;
};

enum { eof_action_eof_code, eof_action_error, eof_action_reset };

struct stream_ {
	union {
		FILE *fp;
		skiplist *keyval;
		query *engine;
	};

	string_buffer sb_buf;
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
	int64_t i_empty;
	double d_empty;
	int ungetch, srclen, rows, cols;
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
	bool is_memory:1;
	bool is_map:1;
	bool is_engine:1;
};

struct page_ {
	page *next;
	cell *heap;
	pl_idx hp, max_hp_used, h_size;
	unsigned nbr;
};

enum q_retry { QUERY_OK=0, QUERY_SKIP=1, QUERY_RETRY=2, QUERY_EXCEPTION=3 };
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
	bool not_strict_iso:1;
	bool debug:1;
	bool json:1;
	bool var_prefix:1;
};

struct query_ {
	query *prev, *next, *parent;
	module *save_m, *current_m;
	prolog *pl;
	parser *p;
	frame *frames;
	slot *slots;
	choice *choices;
	trail *trails;
	cell *tmp_heap, *last_arg, *variable_names, *ball, *suspect;
	cell *queue[MAX_QUEUES], *tmpq[MAX_QUEUES];
	page *pages;
	slot *save_e;
	db_entry *dirty_list;
	query *tasks;
	skiplist *vars;
	cell accum;
	mpz_t tmp_ival;
	mpq_t tmp_irat;
	prolog_state st;
	bool ignores[MAX_IGNORES];
	uint64_t tot_goals, tot_backtracks, tot_retries, tot_matches;
	uint64_t tot_tcos, tot_frecovs, tot_srecovs;
	uint64_t step, qid, tmo_msecs, cgen;
	uint64_t get_started, autofail_n, yield_at;
	uint64_t time_cpu_started, time_cpu_last_started, future;
	unsigned max_depth, max_eval_depth, print_idx, tab_idx, varno, tab0_varno, curr_engine;
	pl_idx tmphp, latest_ctx, popp, variable_names_ctx;
	pl_idx frames_size, slots_size, trails_size, choices_size;
	pl_idx hw_choices, hw_frames, hw_slots, hw_trails;
	pl_idx cp, before_hook_tp, qcnt[MAX_QUEUES];
	pl_idx h_size, tmph_size, tot_heaps, tot_heapsize, undo_lo_tp, undo_hi_tp;
	pl_idx q_size[MAX_QUEUES], tmpq_size[MAX_QUEUES], qp[MAX_QUEUES];
	prolog_flags flags;
	enum q_retry retry;
	uint32_t vgen;
	int8_t halt_code;
	int8_t quoted;
	enum { WAS_OTHER, WAS_SPACE, WAS_COMMA, WAS_SYMBOL } last_thing;
	bool done:1;
	bool parens:1;
	bool in_attvar_print:1;
	bool lists_ok:1;
	bool fail_on_retry:1;
	bool noretry:1;
	bool is_oom:1;
	bool is_redo:1;
	bool run_hook:1;
	bool in_hook:1;
	bool do_dump_vars:1;
	bool is_dump_vars:1;
	bool portray_vars:1;
	bool status:1;
	bool no_tco:1;
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
	bool ignore_ops:1;
	bool numbervars:1;
	bool halt:1;
	bool abort:1;
	bool cycle_error:1;
	bool spawned:1;
	bool run_init:1;
	bool varnames:1;
	bool listing:1;
	bool in_commit:1;
	bool did_quote:1;
	bool is_input:1;
	bool is_engine:1;
	bool ops_dirty:1;
	bool noderef:1;
	bool double_quotes:1;
	bool end_wait:1;
};

struct parser_ {
	struct {
		char var_pool[MAX_VAR_POOL_SIZE];
		unsigned var_used[MAX_VARS];
		const char *var_name[MAX_VARS];
		bool var_in_body[MAX_VARS];
		uint8_t vars[MAX_VARS];
	} vartab;

	prolog *pl;
	FILE *fp;
	module *m;
	clause *cl;
	cell v;
	string_buffer token_buf;
	prolog_flags flags;
	char *save_line, *srcptr, *error_desc;
	size_t token_size, n_line, pos_start;
	unsigned line_nbr, line_nbr_start;
	unsigned depth, read_term_slots, nbr_vars;
	unsigned nesting_parens, nesting_braces, nesting_brackets;
	int quote_char;
	int8_t dq_consing;
	bool error, if_depth[MAX_IF_DEPTH];
	bool was_consing:1;
	bool was_string:1;
	bool did_getline:1;
	bool already_loaded_error:1;
	bool do_read_term:1;
	bool string:1;
	bool run_init:1;
	bool directive:1;
	bool consulting:1;
	bool internal:1;
	bool one_shot:1;
	bool start_term:1;
	bool end_of_term:1;
	bool comment:1;
	bool is_quoted:1;
	bool is_var:1;
	bool is_op:1;
	bool skip:1;
	bool command:1;
	bool last_close:1;
	bool no_fp:1;
	bool symbol:1;
	bool reuse:1;
	bool interactive:1;
};

typedef struct loaded_file_ loaded_file;

struct module_ {
	module *used[MAX_MODULES];
	module *next, *orig;
	prolog *pl;
	const char *filename, *name, *actual_filename;
	predicate *head, *tail;
	parser *p;
	FILE *fp;
	skiplist *index, *ops, *defops;
	loaded_file *loaded_files;
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
	bool make:1;
};

typedef struct {
	pl_idx ctx, val_off;
	unsigned var_nbr, cnt;
	bool is_anon;
} var_item;

struct prolog_ {
	stream streams[MAX_STREAMS];
	module *modmap[MAX_MODULES];
	struct { pl_idx tab1[MAX_IGNORES], tab2[MAX_IGNORES]; };
	char tmpbuf[8192];
	module *modules, *system_m, *user_m, *curr_m, *dcgs;
	var_item *tabs;
	parser *p;
	query *curr_query;
	skiplist *symtab, *biftab, *keyval, *help, *fortab;
	FILE *logfp;
	char *pool;
	size_t pool_offset, pool_size, tabs_size;
	uint64_t s_last, s_cnt, seed, ugen;
	unsigned next_mod_id;
	uint8_t current_input, current_output, current_error;
	int8_t halt_code, opt;
	bool is_redo:1;
	bool is_query:1;
	bool halt:1;
	bool status:1;
	bool error:1;
	bool did_dump_vars:1;
	bool quiet:1;
	bool noindex:1;
	bool iso_only:1;
	bool trace:1;
};

extern pl_idx g_empty_s, g_pair_s, g_dot_s, g_cut_s, g_nil_s, g_true_s, g_fail_s;
extern pl_idx g_anon_s, g_neck_s, g_eof_s, g_lt_s, g_false_s, g_once_s;
extern pl_idx g_gt_s, g_eq_s, g_sys_elapsed_s, g_sys_queue_s, g_braces_s;
extern pl_idx g_sys_stream_property_s, g_unify_s, g_on_s, g_off_s, g_sys_var_s;
extern pl_idx g_call_s, g_braces_s, g_plus_s, g_minus_s, g_post_unify_hook_s;

extern unsigned g_cpu_count;

#define share_cell(c) if (is_managed(c)) share_cell_(c)
#define unshare_cell(c) if (is_managed(c)) unshare_cell_(c)

inline static void share_cell_(const cell *c)
{
	if (is_strbuf(c))
		(c)->val_strb->refcnt++;
	else if (is_bigint(c))
		(c)->val_bigint->refcnt++;
	else if (is_rational(c))
		(c)->val_bigint->refcnt++;
	else if (is_blob(c))
		(c)->val_blob->refcnt++;
}

inline static void unshare_cell_(cell *c)
{
	if (is_strbuf(c)) {
		if (--(c)->val_strb->refcnt == 0) {
			free((c)->val_strb);
			c->flags = 0;
			g_string_cnt--;
		}
	} else if (is_bigint(c)) {
		if (--(c)->val_bigint->refcnt == 0)	{
			mp_int_clear(&(c)->val_bigint->ival);
			free((c)->val_bigint);
			c->flags = 0;
		}
	} else if (is_rational(c)) {
		if (--(c)->val_bigint->refcnt == 0)	{
			mp_rat_clear(&(c)->val_bigint->irat);
			free((c)->val_bigint);
			c->flags = 0;
		}
	} else if (is_blob(c)) {
		if (--(c)->val_blob->refcnt == 0) {
			free((c)->val_blob->ptr2);
			free((c)->val_blob->ptr);
			free((c)->val_blob);
			c->flags = 0;
		}
	}
}

inline static pl_idx copy_cells(cell *dst, const cell *src, pl_idx nbr_cells)
{
	memcpy(dst, src, sizeof(cell)*(nbr_cells));
	return nbr_cells;
}

inline static pl_idx move_cells(cell *dst, const cell *src, pl_idx nbr_cells)
{
	memmove(dst, src, sizeof(cell)*(nbr_cells));
	return nbr_cells;
}

inline static pl_idx safe_copy_cells(cell *dst, const cell *src, pl_idx nbr_cells)
{
	memcpy(dst, src, sizeof(cell)*nbr_cells);

	for (pl_idx i = 0; i < nbr_cells; i++, src++)
		share_cell(src);

	return nbr_cells;
}

inline static void chk_cells(cell *src, pl_idx nbr_cells)
{
	for (pl_idx i = 0; i < nbr_cells; i++, src++)
		unshare_cell(src);
}

#define LIST_HANDLER(l) cell l##_h_tmp, l##_t_tmp
#define LIST_HEAD(l) list_head(l, &l##_h_tmp)
#define LIST_TAIL(l) list_tail(l, &l##_t_tmp)

cell *list_head(cell *l, cell *tmp);
cell *list_tail(cell *l, cell *tmp);

enum clause_type { DO_CLAUSE, DO_RETRACT, DO_RETRACTALL };

size_t formatted(char *dst, size_t dstlen, const char *src, int srclen, bool dq, bool json);
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

#define delink(l, e) {											\
	if (e->prev) e->prev->next = e->next;						\
	if (e->next) e->next->prev = e->prev;						\
	if (l->head == e) l->head = e->next;						\
	if (l->tail == e) l->tail = e->prev;						\
}
