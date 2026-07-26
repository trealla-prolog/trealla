// Native tabling support: Phase 1, the trie.
//
// A trie maps *canonical* term images to values. Canonicalization walks
// the (dereferenced) term left-to-right emitting one key cell per step;
// unbound variables are numbered by first appearance, which makes trie
// lookup variant-matching for free: f(X,Y) and f(A,B) take the same
// path, f(X,X) takes a different one.
//
// Nodes are first-child/next-sibling; fanout in tabled-call workloads is
// small, so linear sibling search beats per-node hashing until proven
// otherwise by a profiler.
//
// Sharp edge, documented: a cstr "string" cell and the equivalent
// cons-list of chars are DIFFERENT trie paths. Within one program the
// representation is consistent (copy_term preserves it), so this only
// matters for mixed-representation calls. Revisit if it ever bites.

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "module.h"
#include "query.h"
#include "heap.h"

typedef struct tnode_ {
	cell key;
	struct tnode_ *child, *sibling;	// all children, insertion-linked
	struct tnode_ *hnext;		// hash-bucket chain (when indexed)
	struct thash_ *index;		// hash over THIS node's children, or NULL
	unsigned nchildren;
	void *value;			// table handle; unused by the tests
	bool is_leaf;
} tnode;

// Above this many children a node's child list gets a hash index:
// linear sibling scans are quadratic for flat key spaces (a tabled
// fib(N,_) puts every distinct N at the same level).

#define TRIE_INDEX_THRESHOLD 16

typedef struct thash_ {
	tnode **buckets;
	unsigned nbuckets, count;
} thash;

// ---------------------------------------------------------------------
// Key comparison. Keys are single canonical cells: TAG_VAR cells carry a
// canonical number in var_num; interned cells compare on functor+arity
// (num_cells is position-dependent and is normalized to 1 at emit time);
// numbers compare by value (small/big int cross-compare included); text
// atoms compare equal whether interned or cstring, but char-list strings
// only match char-list strings.

static bool key_is_atomish(const cell *c)
{
	return (is_interned(c) && !c->arity) || (is_cstring(c) && !is_string(c));
}

static bool key_eq(query *q, const cell *a, const cell *b)
{
	if (is_var(a) || is_var(b))
		return is_var(a) && is_var(b) && (a->var_num == b->var_num);

	// Same-text atoms may arrive interned or as cstrings.

	if (key_is_atomish(a) && key_is_atomish(b)) {
		if (is_interned(a) && is_interned(b))
			return a->val_off == b->val_off;

		size_t al = C_STRLEN(q, a), bl = C_STRLEN(q, b);
		return (al == bl) && !memcmp(C_STR(q, a), C_STR(q, b), al);
	}

	if (a->tag != b->tag)
		return false;

	switch (a->tag) {
	case TAG_INTERNED:
		return (a->val_off == b->val_off) && (a->arity == b->arity);
	case TAG_INT:
		if (is_bigint(a) && is_bigint(b))
			return mp_int_compare(&a->val_bigint->ival, &b->val_bigint->ival) == 0;
		if (is_bigint(a))
			return mp_int_compare_value(&a->val_bigint->ival, b->val_int) == 0;
		if (is_bigint(b))
			return mp_int_compare_value(&b->val_bigint->ival, a->val_int) == 0;
		return a->val_int == b->val_int;
	case TAG_FLOAT: {
		// Bit compare, NOT ==: key_hash hashes the bit pattern, so C
		// equality would merge 0.0/-0.0 (and never match NaN) while the
		// hash separates them - an inconsistency that makes indexed
		// lookups miss. Bitwise is also the right identity for a table
		// key (variant, not arithmetic, equality).
		union { double d; uint64_t u; } ua = { .d = a->val_float };
		union { double d; uint64_t u; } ub = { .d = b->val_float };
		return ua.u == ub.u;
	}
	case TAG_RATIONAL:
		return mp_rat_compare(&a->val_bigint->irat, &b->val_bigint->irat) == 0;
	case TAG_CSTR: {
		if (is_string(a) != is_string(b))
			return false;
		size_t al = C_STRLEN(q, a), bl = C_STRLEN(q, b);
		return (al == bl) && !memcmp(C_STR(q, a), C_STR(q, b), al);
	}
	default:
		return false;
	}
}

// Hash consistent with key_eq: atomish keys (interned atoms and plain
// cstrings) hash their TEXT so both representations collide correctly.

static unsigned key_hash(query *q, const cell *c)
{
	if (is_var(c))
		return 0x9e3779b9u ^ c->var_num;

	if (key_is_atomish(c) || (is_cstring(c) && is_string(c))) {
		const char *p = C_STR(q, c);
		size_t len = C_STRLEN(q, c);
		unsigned h = 2166136261u;

		for (size_t i = 0; i < len; i++)
			h = (h ^ (unsigned char)p[i]) * 16777619u;

		return is_string(c) ? h ^ 0x51175117u : h;
	}

	if (is_interned(c))
		return (unsigned)c->val_off * 33u + c->arity;

	switch (c->tag) {
	case TAG_INT:
		if (is_bigint(c)) {
			mp_int z = &c->val_bigint->ival;
			unsigned h = 0x811c9dc5u;

			for (mp_size i = 0; i < MP_USED(z); i++)
				h = (h ^ (unsigned)MP_DIGITS(z)[i]) * 16777619u;

			return h;
		}

		return (unsigned)(c->val_int ^ (c->val_int >> 32));
	case TAG_FLOAT: {
		union { double d; uint64_t u; } u = { .d = c->val_float };
		return (unsigned)(u.u ^ (u.u >> 32));
	}
	case TAG_RATIONAL: {
		mp_rat r = &c->val_bigint->irat;
		unsigned h = 0x811c9dc5u;

		for (mp_size i = 0; i < MP_USED(&r->num); i++)
			h = (h ^ (unsigned)MP_DIGITS(&r->num)[i]) * 16777619u;

		for (mp_size i = 0; i < MP_USED(&r->den); i++)
			h = (h ^ (unsigned)MP_DIGITS(&r->den)[i]) * 16777619u;

		return h;
	}
	default:
		return 0;
	}
}

static void thash_insert(thash *h, tnode *n, unsigned hv)
{
	unsigned b = hv % h->nbuckets;
	n->hnext = h->buckets[b];
	h->buckets[b] = n;
	h->count++;
}

static bool thash_grow(query *q, thash *h)
{
	unsigned nb = h->nbuckets * 2;
	tnode **nbk = calloc(nb, sizeof(tnode*));
	if (!nbk) return false;
	tnode **old = h->buckets;
	unsigned oldn = h->nbuckets;
	h->buckets = nbk;
	h->nbuckets = nb;
	h->count = 0;

	for (unsigned i = 0; i < oldn; i++) {
		for (tnode *n = old[i]; n; ) {
			tnode *next = n->hnext;
			thash_insert(h, n, key_hash(q, &n->key));
			n = next;
		}
	}

	free(old);
	return true;
}

static bool trie_index_children(query *q, tnode *parent)
{
	thash *h = calloc(1, sizeof(thash));
	if (!h) return false;
	h->nbuckets = 64;
	h->buckets = calloc(h->nbuckets, sizeof(tnode*));
	if (!h->buckets) { free(h); return false; }

	for (tnode *n = parent->child; n; n = n->sibling)
		thash_insert(h, n, key_hash(q, &n->key));

	parent->index = h;
	return true;
}

// ---------------------------------------------------------------------
// Walk state: canonical variable numbering + trie cursor.

typedef struct {
	query *q;
	tnode **root;			// where the trie hangs
	tnode *node;			// last node stepped onto
	struct { pl_ctx ctx; unsigned num; } *vars;
	unsigned num_vars, max_vars;
	bool create;			// insert vs lookup
	bool created_any;
	bool oom;
	bool attvar;			// hit an attributed variable
} twalk;

static void twalk_init(twalk *w, query *q, tnode **root, bool create)
{
	memset(w, 0, sizeof(*w));
	w->q = q;
	w->root = root;
	w->create = create;
}

static void twalk_done(twalk *w)
{
	if (w->vars)
		free(w->vars);
}

static int twalk_var_num(twalk *w, pl_ctx ctx, unsigned var_num)
{
	for (unsigned i = 0; i < w->num_vars; i++) {
		if ((w->vars[i].ctx == ctx) && (w->vars[i].num == var_num))
			return (int)i;
	}

	if (w->num_vars >= w->max_vars) {
		w->max_vars = w->max_vars ? w->max_vars*2 : 16;
		void *tmp = realloc(w->vars, sizeof(w->vars[0]) * w->max_vars);
		if (!tmp) { w->oom = true; return -1; }
		w->vars = tmp;
	}

	w->vars[w->num_vars].ctx = ctx;
	w->vars[w->num_vars].num = var_num;
	return (int)w->num_vars++;
}

// Descend one step on the canonical key cell, creating the node when
// inserting. Returns false when lookup misses (or OOM).

static bool trie_step(twalk *w, const cell *key)
{
	tnode *parent = w->node;
	tnode **slot = parent ? &parent->child : w->root;
	thash *h = parent ? parent->index : NULL;

	if (h) {
		unsigned hv = key_hash(w->q, key);

		for (tnode *n = h->buckets[hv % h->nbuckets]; n; n = n->hnext) {
			if (key_eq(w->q, &n->key, key)) {
				w->node = n;
				return true;
			}
		}
	} else {
		for (tnode *n = *slot; n; n = n->sibling) {
			if (key_eq(w->q, &n->key, key)) {
				w->node = n;
				return true;
			}
		}
	}

	if (!w->create)
		return false;

	tnode *n = calloc(1, sizeof(tnode));
	if (!n) { w->oom = true; return false; }
	n->key = *key;
	share_cell(&n->key);		// bigints/cstrings are refcounted
	n->sibling = *slot;
	*slot = n;
	w->node = n;
	w->created_any = true;

	if (parent) {
		parent->nchildren++;

		if (h) {
			if (h->count >= h->nbuckets - h->nbuckets/4) {
				if (!thash_grow(w->q, h)) { w->oom = true; return false; }
			}

			thash_insert(h, n, key_hash(w->q, key));
		} else if (parent->nchildren > TRIE_INDEX_THRESHOLD) {
			if (!trie_index_children(w->q, parent)) { w->oom = true; return false; }
		}
	}

	return true;
}

// Emit the canonical key sequence for (c, ctx). Recursive on term depth;
// tabled calls are shallow. Returns false on lookup miss / unsupported.

static bool trie_walk(twalk *w, cell *c, pl_ctx ctx)
{
	c = deref(w->q, c, ctx);
	ctx = w->q->latest_ctx;

	if (is_var(c)) {
		// Attributed variables cannot be represented in a call variant:
		// the attributes are not part of the term, so two calls with
		// different constraints would share a table. Reject rather than
		// silently give wrong answers.
		{
			query *q = w->q;
			const frame *f = GET_FRAME(ctx);
			const slot *e = get_slot(q, f, c->var_num);

			if (e->c.val_attrs) {
				w->attvar = true;
				return false;
			}
		}

		int n = twalk_var_num(w, ctx, c->var_num);
		if (n < 0) return false;
		cell key = {0};
		key.tag = TAG_VAR;
		key.num_cells = 1;
		key.var_num = (unsigned)n;
		return trie_step(w, &key);
	}

	if (is_interned(c)) {
		cell key = *c;
		key.num_cells = 1;		// position-dependent; normalize
		key.flags = 0;			// strip OP etc annotations
		key.match = NULL;

		if (!trie_step(w, &key))
			return false;

		cell *arg = c + 1;

		for (unsigned i = 0; i < c->arity; i++) {
			if (!trie_walk(w, arg, ctx))
				return false;

			arg += arg->num_cells;
		}

		return true;
	}

	switch (c->tag) {
	case TAG_INT:
	case TAG_FLOAT:
	case TAG_RATIONAL:
	case TAG_CSTR: {
		cell key = *c;
		key.num_cells = 1;
		return trie_step(w, &key);
	}
	default:
		return false;			// blobs, streams etc: not tableable
	}
}

// Full-term insert: returns leaf node, sets *existed when this exact
// canonical term had been inserted before (the dedup signal).

static tnode *trie_insert_(query *q, tnode **root, cell *c, pl_ctx ctx, bool *existed, bool *attvar)
{
	twalk w;
	twalk_init(&w, q, root, true);
	bool ok = trie_walk(&w, c, ctx);
	tnode *leaf = w.node;
	bool fresh = w.created_any;
	if (attvar) *attvar = w.attvar;
	twalk_done(&w);

	if (!ok || !leaf)
		return NULL;

	*existed = !fresh && leaf->is_leaf;
	leaf->is_leaf = true;
	return leaf;
}

static tnode *trie_insert(query *q, tnode **root, cell *c, pl_ctx ctx, bool *existed)
{
	return trie_insert_(q, root, c, ctx, existed, NULL);
}

// Full-term lookup: NULL when no such canonical path/leaf.

static tnode *trie_lookup(query *q, tnode **root, cell *c, pl_ctx ctx)
{
	twalk w;
	twalk_init(&w, q, root, false);
	bool ok = trie_walk(&w, c, ctx);
	tnode *leaf = w.node;
	twalk_done(&w);
	return (ok && leaf && leaf->is_leaf) ? leaf : NULL;
}

static void trie_free(tnode *n)
{
	while (n) {
		tnode *sib = n->sibling;
		trie_free(n->child);

		if (n->index) {
			free(n->index->buckets);
			free(n->index);
		}

		unshare_cell(&n->key);
		free(n);
		n = sib;
	}
}

static unsigned trie_count_leaves(const tnode *n)
{
	unsigned cnt = 0;

	for (; n; n = n->sibling) {
		if (n->is_leaf)
			cnt++;

		cnt += trie_count_leaves(n->child);
	}

	return cnt;
}

// ---------------------------------------------------------------------
// Tables. A table owns an answer trie (dedup), the answers in insertion
// order, suspended consumers (detached continuation images), and its
// scheduling state. Handles cross the Prolog boundary as integers -
// they only ever flow between '$tbl_*' builtins.
//
// Scheduling: adding an answer to a table that has suspensions (or a
// suspension to a table that has answers) enqueues the table on the
// global worklist. '$tbl_pop_worklist' MATERIALIZES the new work - the
// cartesian pairs (new answers x all suspensions) + (old answers x new
// suspensions) - and resets the cursors, so work arriving during a
// drain is simply picked up by a later enqueue/pop round (batched
// scheduling, like the library's worklists but without the attvars).

enum { TBL_FRESH=0, TBL_ACTIVE=1, TBL_COMPLETE=2 };

typedef struct tbl_ans_ {
	cell *image;
	struct tbl_ans_ *next;
} tbl_ans;

typedef struct tbl_susp_ {
	cell *image;
	struct tbl_susp_ *next;
} tbl_susp;

typedef struct tbl_pair_ {
	tbl_ans *a;
	tbl_susp *s;
	struct tbl_pair_ *next;
} tbl_pair;

typedef struct table_ {
	tnode *answers;			// dedup trie over answer terms
	tbl_ans *first_ans, *last_ans;
	tbl_susp *first_susp, *last_susp;
	tbl_ans *unproc_ans;		// first answer not yet paired, or NULL
	tbl_susp *unproc_susp;		// first suspension not yet paired, or NULL
	tbl_pair *pending;		// materialized work for '$tbl_wkl_work'
	int status;
	bool in_wl;
	unsigned scc;			// owning SCC id (0 = none yet)
	struct table_ *wl_next, *all_next, *fresh_next;
} table;

// Strongly-connected-component stack.
//
// A fresh variant called underneath a running leader is COMPLETED in its
// own nested SCC rather than suspending the consumer. That matters
// because a consumer cannot always be suspended: a tabled call inside
// findall/3 or setof/3 has its continuation buried in the collector's
// C-level state, so a captured goal-list continuation cannot resume it.
// Completing the subgoal avoids the suspension entirely, which is also
// what SWI-Prolog does.
//
// Suspension is then needed only for a genuine cycle - a call to a
// variant that is already ACTIVE (an ancestor). If such a suspension
// targets a table belonging to an OUTER SCC, this SCC is not independent
// after all: on pop its tables are merged into the parent (SCC merging)
// and the parent finishes them.

typedef struct {
	unsigned id;
	table *fresh_head;		// tables owned by this SCC
	unsigned dep_min;		// smallest outer SCC id depended on (0 = none)
} tscc;

static tscc *g_scc = NULL;
static unsigned g_scc_depth = 0, g_scc_max = 0, g_scc_next_id = 1;

static unsigned tbl_scc_id(void)
{
	return g_scc_depth ? g_scc[g_scc_depth-1].id : 0;
}

// Bumped by abolish; enumerations carry the generation they started in
// and stop cleanly if the tables were pulled out from under them.

// int64_t (not uint64_t) to match q->st.v2, where enumerations stash it.

static int64_t g_generation = 1;

// Non-zero while a leader is driving completion. abolish_all_tables/0
// must not free tables that live frames are still enumerating.

static unsigned g_in_use = 0;

// Set when a worker raised an exception during the current leader's
// fixpoint. Such a fixpoint may have gathered only part of the answers,
// so its tables must NOT be cached as complete.

static bool g_saw_exception = false;

static tnode *g_variants = NULL;
static table *g_all_tables = NULL, *g_wl_head = NULL, *g_fresh_head = NULL;
static bool g_leader = false;

static pl_idx s_fresh, s_active, s_complete;

static void tbl_intern_atoms(query *q)
{
	if (!s_fresh) {
		s_fresh = new_atom(q->pl, "fresh");
		s_active = new_atom(q->pl, "active");
		s_complete = new_atom(q->pl, "complete");
	}
}

// Detached term image, the '$bb_put' pattern: one consistent copy, then
// malloc'd cells (managed subcells refcounted by dup_cells).

static cell *tbl_image(query *q, cell *c, pl_ctx ctx)
{
	if (!init_tmp_heap(q))
		return NULL;

	cell *tmp = copy_term_to_tmp(q, c, ctx, false);
	if (!tmp) return NULL;
	cell *val = TPL_malloc(sizeof(cell)*tmp->num_cells);
	if (!val) return NULL;
	dup_cells(val, tmp, tmp->num_cells);
	return val;
}

static void tbl_image_free(cell *c)
{
	if (!c) return;
	unshare_cells(c, c->num_cells);
	TPL_free(c);
}

static void tbl_free_pending(table *t)
{
	tbl_pair *p = t->pending;

	while (p) {
		tbl_pair *next = p->next;
		free(p);
		p = next;
	}

	t->pending = NULL;
}

static void tbl_enqueue(table *t)
{
	if (t->in_wl)
		return;

	t->in_wl = true;
	t->wl_next = g_wl_head;
	g_wl_head = t;
}

static void tbl_destroy(table *t)
{
	trie_free(t->answers);

	for (tbl_ans *a = t->first_ans; a; ) {
		tbl_ans *next = a->next;
		tbl_image_free(a->image);
		free(a);
		a = next;
	}

	for (tbl_susp *s = t->first_susp; s; ) {
		tbl_susp *next = s->next;
		tbl_image_free(s->image);
		free(s);
		s = next;
	}

	tbl_free_pending(t);
	free(t);
}

static table *tbl_handle(cell *c)
{
	return (table*)(size_t)c->val_uint;
}

// Does this term image contain variables? Imported answers that are
// ground need no frame protection (see tbl_pin_answer_frame below), and
// ground answers are the common case, so this keeps the fast path free.

static bool tbl_has_vars(const cell *c)
{
	const cell *p = c;

	for (pl_idx i = 0; i < c->num_cells; i++, p++) {
		if (is_var(p))
			return true;
	}

	return false;
}

// import_term creates an imported term's variables in the CURRENT frame.
// Once unified into the caller's term, any structure the caller now
// holds (eg. an answer argument that is a list containing a shared
// variable) points at slots of THIS frame. Trimming the frame on
// deterministic exit recycles those slots and silently breaks variable
// identity: two occurrences of one answer variable stop being the same
// variable, so binding one no longer binds the other.
//
// The VM protects the analogous case itself - set_var() raises
// q->no_recov when a caller variable is bound to a non-ground compound
// living in a younger frame - but that flag is only transferred to the
// next frame created, so a binding performed inside a builtin never
// reaches the frame that owns the variables. Pin it explicitly.

static void tbl_pin_answer_frame(query *q, const cell *tmp)
{
	if (!tbl_has_vars(tmp))
		return;

	frame *f = GET_FRAME(q->st.cur_ctx);
	f->no_recov = true;
}

// '$tbl_variant_table'(+Variant, -Handle, -Status)

static bool bif_tbl_variant_table_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);
	tbl_intern_atoms(q);
	bool existed = false, attvar = false;
	tnode *leaf = trie_insert_(q, &g_variants, p1, p1_ctx, &existed, &attvar);

	if (!leaf) {
		if (attvar)
			return throw_error(q, p1, p1_ctx, "type_error", "free_variable");

		return throw_error(q, p1, p1_ctx, "representation_error", "tabled_call");
	}

	table *t = leaf->value;

	if (!t) {
		t = calloc(1, sizeof(table));
		CHECKED(t);
		t->status = TBL_FRESH;
		t->all_next = g_all_tables;
		g_all_tables = t;
		leaf->value = t;
	}

	cell tmp;
	make_uint(&tmp, (pl_uint)(size_t)t);

	if (!unify(q, p2, p2_ctx, &tmp, q->st.cur_ctx))
		return false;

	make_atom(&tmp, t->status == TBL_FRESH ? s_fresh :
		t->status == TBL_ACTIVE ? s_active : s_complete);
	return unify(q, p3, p3_ctx, &tmp, q->st.cur_ctx);
}

// '$tbl_set_status'(+Handle, +Status)

static bool bif_tbl_set_status_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,atom);
	tbl_intern_atoms(q);
	table *t = tbl_handle(p1);

	if (p2->val_off == s_fresh) t->status = TBL_FRESH;
	else if (p2->val_off == s_active) t->status = TBL_ACTIVE;
	else if (p2->val_off == s_complete) t->status = TBL_COMPLETE;
	else return throw_error(q, p2, p2_ctx, "domain_error", "table_status");

	return true;
}

// '$tbl_add_answer'(+Handle, +Answer) - semidet: FAILS on duplicate.

static bool bif_tbl_add_answer_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,any);
	table *t = tbl_handle(p1);
	bool existed = false, attvar = false;
	tnode *leaf = trie_insert_(q, &t->answers, p2, p2_ctx, &existed, &attvar);

	if (!leaf) {
		if (attvar)
			return throw_error(q, p2, p2_ctx, "type_error", "free_variable");

		return throw_error(q, p2, p2_ctx, "representation_error", "tabled_answer");
	}

	if (existed)
		return false;

	tbl_ans *a = calloc(1, sizeof(tbl_ans));
	CHECKED(a);
	a->image = tbl_image(q, p2, p2_ctx);
	CHECKED(a->image);

	if (t->last_ans) t->last_ans->next = a; else t->first_ans = a;
	t->last_ans = a;

	if (!t->unproc_ans)
		t->unproc_ans = a;

	if (t->first_susp)
		tbl_enqueue(t);

	return true;
}

// '$tbl_get_answer'(+Handle, ?Answer) - nondet enumeration.

static bool bif_tbl_get_answer_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,any);
	table *t = tbl_handle(p1);

	// Stop cleanly if the tables were abolished under a live enumeration.

	if (q->retry && (q->st.v2 != g_generation))
		return false;

	tbl_ans *a = q->retry ? (tbl_ans*)(size_t)q->st.v1 : t->first_ans;

	if (!a)
		return false;

	if (a->next) {
		q->st.v1 = (uint64_t)(size_t)a->next;
		q->st.v2 = g_generation;
		CHECKED(push_choice(q));
	}

	cell *tmp = import_term(q, a->image, q->st.cur_ctx);
	CHECKED(tmp);
	tbl_pin_answer_frame(q, tmp);
	return unify(q, p2, p2_ctx, tmp, q->st.cur_ctx);
}

// '$tbl_add_suspension'(+Handle, +Dependency)

static bool bif_tbl_add_suspension_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,any);
	table *t = tbl_handle(p1);

	// Depending on a table owned by an outer SCC means this SCC cannot
	// complete on its own (see the SCC comment above).

	if (g_scc_depth && t->scc && (t->scc < tbl_scc_id())) {
		tscc *top = &g_scc[g_scc_depth-1];

		if (!top->dep_min || (t->scc < top->dep_min))
			top->dep_min = t->scc;
	}

	tbl_susp *s = calloc(1, sizeof(tbl_susp));
	CHECKED(s);
	s->image = tbl_image(q, p2, p2_ctx);
	CHECKED(s->image);

	if (t->last_susp) t->last_susp->next = s; else t->first_susp = s;
	t->last_susp = s;

	if (!t->unproc_susp)
		t->unproc_susp = s;

	if (t->first_ans)
		tbl_enqueue(t);

	return true;
}

// '$tbl_pop_worklist'(-Handle) - semidet; materializes the new work.

static bool bif_tbl_pop_worklist_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	// Only drain tables owned by the SCC we are completing; work for
	// outer SCCs is left for their own completion loops.

	unsigned scc_id = tbl_scc_id();
	table *t = g_wl_head, *prev = NULL;

	while (t && (t->scc != scc_id)) {
		prev = t;
		t = t->wl_next;
	}

	if (!t)
		return false;

	if (prev)
		prev->wl_next = t->wl_next;
	else
		g_wl_head = t->wl_next;

	t->wl_next = NULL;
	t->in_wl = false;

	tbl_free_pending(t);
	tbl_pair **tail = &t->pending;

	// (new answers x all suspensions)

	for (tbl_ans *a = t->unproc_ans; a; a = a->next) {
		for (tbl_susp *s = t->first_susp; s; s = s->next) {
			tbl_pair *p = malloc(sizeof(tbl_pair));
			CHECKED(p);
			p->a = a; p->s = s; p->next = NULL;
			*tail = p; tail = &p->next;
		}
	}

	// (old answers x new suspensions); old = before unproc_ans

	for (tbl_ans *a = t->first_ans; a && a != t->unproc_ans; a = a->next) {
		for (tbl_susp *s = t->unproc_susp; s; s = s->next) {
			tbl_pair *p = malloc(sizeof(tbl_pair));
			CHECKED(p);
			p->a = a; p->s = s; p->next = NULL;
			*tail = p; tail = &p->next;
		}
	}

	t->unproc_ans = NULL;
	t->unproc_susp = NULL;

	cell tmp;
	make_uint(&tmp, (pl_uint)(size_t)t);
	return unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
}

// '$tbl_wkl_work'(+Handle, -Answer, -Dependency) - nondet over the
// materialized pairs; each solution gets fresh copies.

static bool bif_tbl_wkl_work_3(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);
	table *t = tbl_handle(p1);

	if (q->retry && (q->st.v2 != g_generation))
		return false;

	tbl_pair *p = q->retry ? (tbl_pair*)(size_t)q->st.v1 : t->pending;

	if (!p)
		return false;

	if (p->next) {
		q->st.v1 = (uint64_t)(size_t)p->next;
		q->st.v2 = g_generation;
		CHECKED(push_choice(q));
	}

	cell *ta = import_term(q, p->a->image, q->st.cur_ctx);
	CHECKED(ta);
	tbl_pin_answer_frame(q, ta);

	if (!unify(q, p2, p2_ctx, ta, q->st.cur_ctx))
		return false;

	cell *td = import_term(q, p->s->image, q->st.cur_ctx);
	CHECKED(td);
	tbl_pin_answer_frame(q, td);
	return unify(q, p3, p3_ctx, td, q->st.cur_ctx);
}

// Leader flag (the "scheduling component" of the Desouter driver).

static bool bif_tbl_leader_0(query *q) { (void)q; return g_scc_depth != 0; }

// '$tbl_push_scc'(+Handle): open a nested SCC owned by this table.

static bool bif_tbl_push_scc_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	table *t = tbl_handle(p1);

	if (g_scc_depth >= g_scc_max) {
		unsigned nmax = g_scc_max ? g_scc_max*2 : 64;
		tscc *tmp = realloc(g_scc, sizeof(tscc)*nmax);
		CHECKED(tmp);
		g_scc = tmp;
		g_scc_max = nmax;
	}

	tscc *top = &g_scc[g_scc_depth++];
	top->id = g_scc_next_id++;
	top->dep_min = 0;
	top->fresh_head = t;
	t->scc = top->id;
	t->fresh_next = NULL;
	g_in_use++;
	return true;
}

// '$tbl_pop_scc'(-Escaped): close it. Escaped == true means this SCC
// depends on an outer one, so its tables are merged into the parent
// (which will complete them) instead of being completed here.

static bool bif_tbl_pop_scc_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (!g_scc_depth)
		return false;

	tscc *top = &g_scc[--g_scc_depth];

	if (g_in_use)
		g_in_use--;

	bool escaped = top->dep_min != 0;

	if (escaped && g_scc_depth) {
		tscc *parent = &g_scc[g_scc_depth-1];

		// Merge: re-tag and hand our tables to the parent.

		table *t = top->fresh_head, *last = NULL;

		for (; t; t = t->fresh_next) {
			t->scc = parent->id;
			last = t;
		}

		if (last) {
			last->fresh_next = parent->fresh_head;
			parent->fresh_head = top->fresh_head;
		}

		// Still depending further out? Propagate.

		if (top->dep_min < parent->id) {
			if (!parent->dep_min || (top->dep_min < parent->dep_min))
				parent->dep_min = top->dep_min;
		}
	}

	top->fresh_head = NULL;
	cell tmp;
	make_atom(&tmp, escaped ? g_true_s : g_false_s);
	return unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
}

// '$tbl_mark_all_complete' - complete every table created since the
// leader started (the library's "newly created table identifiers").

static bool bif_tbl_mark_all_complete_0(query *q)
{
	(void)q;

	if (!g_scc_depth)
		return true;

	tscc *top = &g_scc[g_scc_depth-1];

	for (table *t = top->fresh_head; t; ) {
		table *next = t->fresh_next;
		t->status = TBL_COMPLETE;
		t->fresh_next = NULL;
		tbl_free_pending(t);

		// A completed table never resumes consumers again: its
		// suspensions (whole continuation images) are dead weight.
		// Freeing here is a large win for many-table workloads.

		for (tbl_susp *sp = t->first_susp; sp; ) {
			tbl_susp *snext = sp->next;
			tbl_image_free(sp->image);
			free(sp);
			sp = snext;
		}

		t->first_susp = t->last_susp = t->unproc_susp = NULL;
		t = next;
	}

	top->fresh_head = NULL;
	g_saw_exception = false;
	return true;
}

// Roll back tables left ACTIVE by an aborted leader so a later call
// re-computes them instead of suspending on a table nobody will finish.

static bool bif_tbl_note_exception_0(query *q)
{
	(void)q;
	g_saw_exception = true;
	return true;
}

static bool bif_tbl_saw_exception_0(query *q)
{
	(void)q;
	return g_saw_exception;
}

static bool bif_tbl_reset_incomplete_0(query *q)
{
	(void)q;

	if (!g_scc_depth)
		return true;

	tscc *rtop = &g_scc[g_scc_depth-1];

	for (table *t = rtop->fresh_head; t; ) {
		table *next = t->fresh_next;

		if (t->status != TBL_COMPLETE) {
			t->status = TBL_FRESH;
			tbl_free_pending(t);

			for (tbl_susp *sp = t->first_susp; sp; ) {
				tbl_susp *snext = sp->next;
				tbl_image_free(sp->image);
				free(sp);
				sp = snext;
			}

			t->first_susp = t->last_susp = t->unproc_susp = NULL;
			t->unproc_ans = t->first_ans;
			t->in_wl = false;
		}

		t->fresh_next = NULL;
		t = next;
	}

	rtop->fresh_head = NULL;
	g_saw_exception = false;
	return true;
}

static bool bif_tbl_abolish_all_tables_0(query *q)
{
	// Freeing tables while a leader is driving completion (or while an
	// enumeration frame is live) would leave dangling handles behind.

	if (g_in_use)
		return throw_error(q, q->st.instr, q->st.cur_ctx, "permission_error", "modify,table");

	g_generation++;

	for (table *t = g_all_tables; t; ) {
		table *next = t->all_next;
		tbl_destroy(t);
		t = next;
	}

	trie_free(g_variants);
	g_variants = NULL;
	g_all_tables = g_wl_head = g_fresh_head = NULL;
	g_leader = false;
	g_scc_depth = 0;
	g_saw_exception = false;
	return true;
}

// ---------------------------------------------------------------------
// Test builtins. A single process-global test trie; the real variant
// trie will hang off prolog* alongside the table registry.

static tnode *g_test_trie = NULL;

static bool bif_sys_trie_test_clear_0(query *q)
{
	(void)q;
	trie_free(g_test_trie);
	g_test_trie = NULL;
	return true;
}

static bool bif_sys_trie_test_insert_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	bool existed = false;

	if (!trie_insert(q, &g_test_trie, p1, p1_ctx, &existed))
		return false;

	cell tmp;
	make_atom(&tmp, existed ? g_true_s : g_false_s);
	return unify(q, p2, p2_ctx, &tmp, q->st.cur_ctx);
}

static bool bif_sys_trie_test_lookup_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return trie_lookup(q, &g_test_trie, p1, p1_ctx) != NULL;
}

static bool bif_sys_trie_test_count_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	cell tmp;
	make_int(&tmp, (pl_int)trie_count_leaves(g_test_trie));
	return unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
}

builtins g_tabling_bifs[] =
{
	{"$tbl_variant_table", 3, bif_tbl_variant_table_3, "+term,-integer,-atom", false, false, BLAH},
	{"$tbl_set_status", 2, bif_tbl_set_status_2, "+integer,+atom", false, false, BLAH},
	{"$tbl_add_answer", 2, bif_tbl_add_answer_2, "+integer,+term", false, false, BLAH},
	{"$tbl_get_answer", 2, bif_tbl_get_answer_2, "+integer,?term", false, false, BLAH},
	{"$tbl_add_suspension", 2, bif_tbl_add_suspension_2, "+integer,+term", false, false, BLAH},
	{"$tbl_pop_worklist", 1, bif_tbl_pop_worklist_1, "-integer", false, false, BLAH},
	{"$tbl_wkl_work", 3, bif_tbl_wkl_work_3, "+integer,-term,-term", false, false, BLAH},
	{"$tbl_leader", 0, bif_tbl_leader_0, "", false, false, BLAH},
	{"$tbl_push_scc", 1, bif_tbl_push_scc_1, "+integer", false, false, BLAH},
	{"$tbl_pop_scc", 1, bif_tbl_pop_scc_1, "-atom", false, false, BLAH},
	{"$tbl_mark_all_complete", 0, bif_tbl_mark_all_complete_0, "", false, false, BLAH},
	{"$tbl_reset_incomplete", 0, bif_tbl_reset_incomplete_0, "", false, false, BLAH},
	{"$tbl_note_exception", 0, bif_tbl_note_exception_0, "", false, false, BLAH},
	{"$tbl_saw_exception", 0, bif_tbl_saw_exception_0, "", false, false, BLAH},
	{"$tbl_abolish_all_tables", 0, bif_tbl_abolish_all_tables_0, "", false, false, BLAH},

	{"$trie_test_clear", 0, bif_sys_trie_test_clear_0, "", false, false, BLAH},
	{"$trie_test_insert", 2, bif_sys_trie_test_insert_2, "+term,-atom", false, false, BLAH},
	{"$trie_test_lookup", 1, bif_sys_trie_test_lookup_1, "+term", false, false, BLAH},
	{"$trie_test_count", 1, bif_sys_trie_test_count_1, "-integer", false, false, BLAH},

	{0}
};
