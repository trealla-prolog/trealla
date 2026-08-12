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

		return (unsigned)((uint64_t)c->val_int ^ ((uint64_t)c->val_int >> 32));
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

static void thash_remove(query *q, thash *h, tnode *n)
{
	tnode **pp = &h->buckets[key_hash(q, &n->key) % h->nbuckets];

	while (*pp) {
		if (*pp == n) {
			*pp = n->hnext;
			h->count--;
			return;
		}

		pp = &(*pp)->hnext;
	}
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

	// The first node this walk created, and where it hangs. Everything
	// created afterwards is inside its subtree, so unlinking this one
	// discards exactly what the walk added - see trie_insert_().

	tnode **first_slot;
	tnode *first_node, *first_parent;
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

	if (!w->created_any) {
		w->first_slot = slot;
		w->first_node = n;
		w->first_parent = parent;
	}

	w->created_any = true;

	if (parent) {
		parent->nchildren++;

		// Both of these are best-effort: the index is only an
		// optimisation, and the sibling chain is always correct on its
		// own. Failing the insert here used to be worse than useless -
		// a failed grow returned before thash_insert(), leaving the new
		// node in the sibling chain but absent from the index, so the
		// next lookup missed it and created a DUPLICATE key. That
		// breaks the dedup the answer trie exists for.

		if (h) {
			if (h->count >= h->nbuckets - h->nbuckets/4)
				thash_grow(w->q, h);		// denser if it fails, still correct

			thash_insert(h, n, key_hash(w->q, key));
		} else if (parent->nchildren > TRIE_INDEX_THRESHOLD)
			trie_index_children(w->q, parent);	// unindexed if it fails
	}

	return true;
}

// Emit the canonical key sequence for (c, ctx). Recursive on term depth;
// tabled calls are shallow. Returns false on lookup miss / unsupported.

static bool trie_walk(twalk *w, cell *c, pl_ctx ctx)
{
	// The last argument is handled by looping rather than recursing.
	// Recursion here is bounded by term depth, and while a tabled CALL
	// is shallow an ANSWER need not be: a list is right-nested '.'/2,
	// so a 100k-element answer recursed 100k frames and overflowed the
	// stack (SIGSEGV, no error term). Iterating on the final argument
	// makes any right-nested term O(1) deep.

	for (;;) {
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

		if (!c->arity)
			return true;

		cell *arg = c + 1;
		const unsigned last = c->arity - 1;	// arity >= 1, checked above

		for (unsigned i = 0; i < last; i++) {
			if (!trie_walk(w, arg, ctx))
				return false;

			arg += arg->num_cells;
		}

		c = arg;				// last argument: loop, don't recurse
		continue;
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
}

// Full-term insert: returns leaf node, sets *existed when this exact
// canonical term had been inserted before (the dedup signal).

static void trie_free(tnode *n);

static tnode *trie_insert_(query *q, tnode **root, cell *c, pl_ctx ctx, bool *existed, bool *attvar)
{
	twalk w;
	twalk_init(&w, q, root, true);
	bool ok = trie_walk(&w, c, ctx);
	tnode *leaf = w.node;
	bool fresh = w.created_any;
	if (attvar) *attvar = w.attvar;
	twalk_done(&w);

	if (!ok || !leaf) {
		// The walk failed part-way - an attributed variable, a blob, or
		// OOM - after possibly creating nodes for the arguments it did
		// get through. Those are unreachable (never marked is_leaf) but
		// they were never reclaimed either, so a program looping on a
		// tabled call with an untabelable answer grew the trie on every
		// throw. Discard what this walk added.

		if (w.first_node) {
			*w.first_slot = w.first_node->sibling;

			if (w.first_parent) {
				w.first_parent->nchildren--;

				if (w.first_parent->index)
					thash_remove(q, w.first_parent->index, w.first_node);
			}

			w.first_node->sibling = NULL;	// don't walk into the live trie
			trie_free(w.first_node);
		}

		return NULL;
	}

	*existed = !fresh && leaf->is_leaf;
	leaf->is_leaf = true;
	return leaf;
}

// Explicit worklist rather than recursion on ->child. Siblings already
// iterated, but children recursed, so the trie for one long-list answer
// - a chain one node wide and as deep as the list is long - overflowed
// the stack on teardown for the same reason trie_walk did on insert.
//
// The nodes are being freed anyway, so ->sibling is reused as the
// worklist link. Each child is pushed individually: linear overall.

static void trie_free(tnode *n)
{
	tnode *stack = n;

	while (stack) {
		tnode *cur = stack;
		stack = cur->sibling;

		for (tnode *ch = cur->child, *next; ch; ch = next) {
			next = ch->sibling;
			ch->sibling = stack;
			stack = ch;
		}

		if (cur->index) {
			free(cur->index->buckets);
			free(cur->index);
		}

		unshare_cell(&cur->key);
		free(cur);
	}
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

	// Identity of the call this table answers, so abolish_table/1 can
	// find every variant of one predicate, and the trie leaf pointing
	// here, so it can be reset to "no table yet" when we drop it.

	pl_idx functor;
	unsigned arity;
	tnode *leaf;
	unsigned slot;			// index into tbl_state.slots (handle identity)

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

// --- per-thread state ---
//
// This was file statics (two prolog instances silently shared tables),
// then per-prolog (one thread per instance could table, the rest got
// resource_error). It is now per-thread: allocated on first tabled
// call, freed by tabling_destroy_thread() when the thread retires and
// by tabling_destroy() sweeping every slot at pl_destroy().

typedef struct {
	tscc *scc;
	unsigned scc_depth, scc_max, scc_next_id;

	// Bumped by abolish; enumerations carry the generation they started
	// in and stop cleanly if the tables were pulled out from under them.
	// int64_t (not uint64_t) to match q->st.v2, where they stash it.

	int64_t generation;

	// Non-zero while a leader is driving completion. Abolishing must not
	// free tables that live frames are still enumerating.

	unsigned in_use;

	// Set when a worker raised an exception during the current leader's
	// fixpoint. Such a fixpoint may have gathered only part of the
	// answers, so its tables must NOT be cached as complete.

	bool saw_exception;

	tnode *variants;
	table *all_tables, *wl_head, *fresh_head;
	bool leader;

	// Handles are (serial, index) pairs into this array rather than raw
	// pointers, so a handle held across abolish_all_tables/0 is detected
	// instead of dereferenced. Releasing a slot bumps its serial, which
	// invalidates every handle that referred to it.

	struct { table *t; uint32_t serial; } *slots;
	unsigned nslots, slots_cap;

#if USE_THREADS
#endif

} tbl_state;

// Per-THREAD state. threads[0] is the main thread - the same
// q->thread_ptr ? : &pl->threads[0] idiom used in query.c, toplevel.c,
// bif_os.c and bif_threads.c.
//
// This is what makes tabling thread-safe without a single lock: a
// table is only ever reachable from the thread that created it, so
// there is nothing to race. The alternative - locking shared tables -
// cannot work here anyway, because the leader's critical section spans
// completion/0, a PROLOG loop that runs arbitrary user code between
// '$tbl_*' calls. No lock can be held across that.
//
// The cost is that threads do not share completed tables and each
// recomputes its own. Sharing completed (hence immutable) tables via a
// published registry is the natural next step and does not disturb
// this invariant.

static tbl_state *tbl(query *q)
{
	thread *self = q->thread_ptr ? q->thread_ptr : &q->pl->threads[0];

	if (!self->tabling_state) {
		tbl_state *s = calloc(1, sizeof(tbl_state));
		if (!s) return NULL;
		s->generation = 1;
		s->scc_next_id = 1;
		self->tabling_state = s;
	}

	return (tbl_state*)self->tabling_state;
}

static unsigned tbl_scc_id(const tbl_state *s)
{
	return s->scc_depth ? s->scc[s->scc_depth-1].id : 0;
}

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

static void tbl_enqueue(tbl_state *s, table *t)
{
	if (t->in_wl)
		return;

	t->in_wl = true;
	t->wl_next = s->wl_head;
	s->wl_head = t;
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

// Table handles cross into Prolog as integers. The '$tbl_*' builtins
// are ordinary entries in the builtins table - the '$' is a naming
// convention, not access control, and the README documents users
// calling other '$' predicates directly - so any program can reach
// them with an integer of its choosing:
//
//     ?- '$tbl_set_status'(0, complete).
//
// That used to cast 0 to a table* and dereference it. Worse, a handle
// obtained legitimately and then used after abolish_all_tables/0 was a
// use-after-free, which ASAN confirms at '$tbl_get_answer'.
//
// So a handle is no longer a pointer. It is a (serial, index) pair into
// tbl_state.slots: the index finds the slot, and the serial must match
// the one the slot held when the handle was made. Releasing a slot
// bumps its serial, so every handle to it stops validating. The cell is
// also tagged FLAG_INT_TABLE, the way streams use FLAG_INT_STREAM, so a
// plain integer is rejected before any of that.

#define TBL_HANDLE(idx, ser) (((pl_uint)(ser) << 32) | (pl_uint)(idx))

static bool tbl_slot_alloc(tbl_state *s, table *t)
{
	for (unsigned i = 0; i < s->nslots; i++) {
		if (!s->slots[i].t) {
			s->slots[i].t = t;
			t->slot = i;
			return true;
		}
	}

	if (s->nslots >= s->slots_cap) {
		unsigned cap = s->slots_cap ? s->slots_cap * 2 : 16;
		void *tmp = realloc(s->slots, sizeof(*s->slots) * cap);
		if (!tmp) return false;
		s->slots = tmp;
		memset(&s->slots[s->slots_cap], 0,
			sizeof(*s->slots) * (cap - s->slots_cap));
		s->slots_cap = cap;
	}

	s->slots[s->nslots].t = t;
	s->slots[s->nslots].serial = 1;
	t->slot = s->nslots++;
	return true;
}

static void tbl_slot_release(tbl_state *s, const table *t)
{
	if (!s->slots || (t->slot >= s->nslots) || (s->slots[t->slot].t != t))
		return;

	s->slots[t->slot].t = NULL;
	s->slots[t->slot].serial++;		// invalidates outstanding handles
}

static void make_tbl_handle(tbl_state *s, cell *tmp, const table *t)
{
	make_uint(tmp, TBL_HANDLE(t->slot, s->slots[t->slot].serial));
	tmp->flags |= FLAG_INT_TABLE;
}

static table *tbl_handle(tbl_state *s, cell *c)
{
	if (!s || !is_integer(c) || !(c->flags & FLAG_INT_TABLE))
		return NULL;

	pl_uint v = c->val_uint;
	unsigned idx = (unsigned)(v & 0xffffffffu);
	uint32_t ser = (uint32_t)(v >> 32);

	if ((idx >= s->nslots) || !s->slots[idx].t
		|| (s->slots[idx].serial != ser))
		return NULL;

	return s->slots[idx].t;
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
	tbl_state *s = tbl(q);
	CHECKED(s);

	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);

	tbl_intern_atoms(q);
	bool existed = false, attvar = false;
	tnode *leaf = trie_insert_(q, &s->variants, p1, p1_ctx, &existed, &attvar);

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
		// A non-interned callable would record functor 0 and then be
		// invisible to abolish_table/1. Callables reaching here are
		// interned in practice; if that ever changes this needs a real
		// key rather than a silent 0.

		t->functor = is_interned(p1) ? p1->val_off : 0;
		t->arity = p1->arity;
		t->leaf = leaf;
		t->all_next = s->all_tables;
		s->all_tables = t;
		leaf->value = t;

		if (!tbl_slot_alloc(s, t)) {
			s->all_tables = t->all_next;
			leaf->value = NULL;
			free(t);
			return throw_error(q, p1, p1_ctx, "resource_error", "memory");
		}
	}

	cell tmp;
	make_tbl_handle(s, &tmp, t);

	if (!unify(q, p2, p2_ctx, &tmp, q->st.cur_ctx))
		return false;

	make_atom(&tmp, t->status == TBL_FRESH ? s_fresh :
		t->status == TBL_ACTIVE ? s_active : s_complete);
	return unify(q, p3, p3_ctx, &tmp, q->st.cur_ctx);
}

// '$tbl_set_status'(+Handle, +Status)

static bool bif_tbl_set_status_2(query *q)
{
	tbl_state *s = tbl(q);
	CHECKED(s);

	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,atom);
	tbl_intern_atoms(q);
	table *t = tbl_handle(s, p1);

	if (!t)
		return throw_error(q, p1, p1_ctx, "type_error", "table_handle");

	if (p2->val_off == s_fresh) t->status = TBL_FRESH;
	else if (p2->val_off == s_active) t->status = TBL_ACTIVE;
	else if (p2->val_off == s_complete) t->status = TBL_COMPLETE;
	else return throw_error(q, p2, p2_ctx, "domain_error", "table_status");

	return true;
}

// '$tbl_add_answer'(+Handle, +Answer) - semidet: FAILS on duplicate.

static bool bif_tbl_add_answer_2(query *q)
{
	tbl_state *s = tbl(q);
	CHECKED(s);

	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,any);
	table *t = tbl_handle(s, p1);

	if (!t)
		return throw_error(q, p1, p1_ctx, "type_error", "table_handle");
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
		tbl_enqueue(s, t);

	return true;
}

// '$tbl_get_answer'(+Handle, ?Answer) - nondet enumeration.

static bool bif_tbl_get_answer_2(query *q)
{
	tbl_state *s = tbl(q);
	CHECKED(s);

	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,any);
	table *t = tbl_handle(s, p1);

	if (!t)
		return throw_error(q, p1, p1_ctx, "type_error", "table_handle");

	// Stop cleanly if the tables were abolished under a live enumeration.

	if (q->retry && (q->st.v2 != s->generation))
		return false;

	tbl_ans *a = q->retry ? (tbl_ans*)(size_t)q->st.v1 : t->first_ans;

	if (!a)
		return false;

	// No choice point on the last answer. That is only sound because a
	// COMPLETE table is immutable: nothing appends after completion, so
	// the answer that has no successor now will not acquire one. Phase 2
	// breaks this in two places - answer subsumption *updates* existing
	// answers, and batched scheduling posts answers before completion -
	// and either would silently drop answers here.

	if (a->next) {
		q->st.v1 = (uint64_t)(size_t)a->next;
		q->st.v2 = s->generation;
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
	tbl_state *s = tbl(q);
	CHECKED(s);

	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,any);
	table *t = tbl_handle(s, p1);

	if (!t)
		return throw_error(q, p1, p1_ctx, "type_error", "table_handle");

	// Depending on a table owned by an outer SCC means this SCC cannot
	// complete on its own (see the SCC comment above).

	if (s->scc_depth && t->scc && (t->scc < tbl_scc_id(s))) {
		tscc *top = &s->scc[s->scc_depth-1];

		if (!top->dep_min || (t->scc < top->dep_min))
			top->dep_min = t->scc;
	}

	tbl_susp *sp = calloc(1, sizeof(tbl_susp));
	CHECKED(sp);
	sp->image = tbl_image(q, p2, p2_ctx);
	CHECKED(sp->image);

	if (t->last_susp) t->last_susp->next = sp; else t->first_susp = sp;
	t->last_susp = sp;

	if (!t->unproc_susp)
		t->unproc_susp = sp;

	if (t->first_ans)
		tbl_enqueue(s, t);

	return true;
}

// '$tbl_pop_worklist'(-Handle) - semidet; materializes the new work.

static bool bif_tbl_pop_worklist_1(query *q)
{
	tbl_state *s = tbl(q);
	CHECKED(s);

	GET_FIRST_ARG(p1,any);

	// Only drain tables owned by the SCC we are completing; work for
	// outer SCCs is left for their own completion loops.

	unsigned scc_id = tbl_scc_id(s);
	table *t = s->wl_head, *prev = NULL;

	while (t && (t->scc != scc_id)) {
		prev = t;
		t = t->wl_next;
	}

	if (!t)
		return false;

	if (prev)
		prev->wl_next = t->wl_next;
	else
		s->wl_head = t->wl_next;

	t->wl_next = NULL;
	t->in_wl = false;

	tbl_free_pending(t);
	tbl_pair **tail = &t->pending;

	// (new answers x all suspensions)

	for (tbl_ans *a = t->unproc_ans; a; a = a->next) {
		for (tbl_susp *sp = t->first_susp; sp; sp = sp->next) {
			tbl_pair *p = malloc(sizeof(tbl_pair));
			CHECKED(p);
			p->a = a; p->s = sp; p->next = NULL;
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
	make_tbl_handle(s, &tmp, t);
	return unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
}

// '$tbl_wkl_work'(+Handle, -Answer, -Dependency) - nondet over the
// materialized pairs; each solution gets fresh copies.

static bool bif_tbl_wkl_work_3(query *q)
{
	tbl_state *s = tbl(q);
	CHECKED(s);

	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);
	table *t = tbl_handle(s, p1);

	if (!t)
		return throw_error(q, p1, p1_ctx, "type_error", "table_handle");

	if (q->retry && (q->st.v2 != s->generation))
		return false;

	tbl_pair *p = q->retry ? (tbl_pair*)(size_t)q->st.v1 : t->pending;

	if (!p)
		return false;

	if (p->next) {
		q->st.v1 = (uint64_t)(size_t)p->next;
		q->st.v2 = s->generation;
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

static bool bif_tbl_leader_0(query *q)
{
	tbl_state *s = tbl(q);
	CHECKED(s);
	return s->scc_depth != 0;
}

// '$tbl_push_scc'(+Handle): open a nested SCC owned by this table.

static bool bif_tbl_push_scc_1(query *q)
{
	tbl_state *s = tbl(q);
	CHECKED(s);

	GET_FIRST_ARG(p1,integer);
	table *t = tbl_handle(s, p1);

	if (!t)
		return throw_error(q, p1, p1_ctx, "type_error", "table_handle");

	if (s->scc_depth >= s->scc_max) {
		unsigned nmax = s->scc_max ? s->scc_max*2 : 64;
		tscc *tmp = realloc(s->scc, sizeof(tscc)*nmax);
		CHECKED(tmp);
		s->scc = tmp;
		s->scc_max = nmax;
	}

	tscc *top = &s->scc[s->scc_depth++];
	top->id = s->scc_next_id++;
	top->dep_min = 0;
	top->fresh_head = t;
	t->scc = top->id;
	t->fresh_next = NULL;
	s->in_use++;
	return true;
}

// '$tbl_pop_scc'(-Escaped): close it. Escaped == true means this SCC
// depends on an outer one, so its tables are merged into the parent
// (which will complete them) instead of being completed here.

static bool bif_tbl_pop_scc_1(query *q)
{
	tbl_state *s = tbl(q);
	CHECKED(s);

	GET_FIRST_ARG(p1,any);

	if (!s->scc_depth)
		return false;

	tscc *top = &s->scc[--s->scc_depth];

	if (s->in_use)
		s->in_use--;

	bool escaped = top->dep_min != 0;

	if (escaped && s->scc_depth) {
		tscc *parent = &s->scc[s->scc_depth-1];

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
	tbl_state *s = tbl(q);
	CHECKED(s);

	(void)q;

	if (!s->scc_depth)
		return true;

	tscc *top = &s->scc[s->scc_depth-1];

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
	s->saw_exception = false;
	return true;
}

// Roll back tables left ACTIVE by an aborted leader so a later call
// re-computes them instead of suspending on a table nobody will finish.

static bool bif_tbl_note_exception_0(query *q)
{
	tbl_state *s = tbl(q);
	CHECKED(s);

	(void)q;
	s->saw_exception = true;
	return true;
}

static bool bif_tbl_saw_exception_0(query *q)
{
	tbl_state *s = tbl(q);
	CHECKED(s);

	(void)q;
	return s->saw_exception;
}

static bool bif_tbl_reset_incomplete_0(query *q)
{
	tbl_state *s = tbl(q);
	CHECKED(s);

	(void)q;

	if (!s->scc_depth)
		return true;

	tscc *rtop = &s->scc[s->scc_depth-1];

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
	s->saw_exception = false;
	return true;
}

// Drop every table. Shared by abolish_all_tables/0 and by
// tabling_destroy() at instance teardown.

static void tbl_clear_all(tbl_state *s)
{
	for (table *t = s->all_tables; t; ) {
		table *next = t->all_next;
		tbl_slot_release(s, t);
		tbl_destroy(t);
		t = next;
	}

	trie_free(s->variants);
	s->variants = NULL;
	s->all_tables = s->wl_head = s->fresh_head = NULL;
	s->leader = false;
	s->scc_depth = 0;

	// The SCC stack is a high-water-mark array that otherwise lives as
	// long as the instance; nothing is on it here, so hand it back.

	free(s->scc);
	s->scc = NULL;
	s->scc_max = 0;
	s->saw_exception = false;
}

// Free one thread's tables. Called when a thread slot retires, so a
// long-lived process spawning many threads does not accumulate them.

void tabling_destroy_thread(thread *t)
{
	tbl_state *s = (tbl_state*)t->tabling_state;

	if (!s)
		return;

	tbl_clear_all(s);
	free(s->slots);
	free(s);
	t->tabling_state = NULL;
}

// Instance teardown: sweep every slot, including threads[0] (the main
// thread) and any that exited without going through the retire path.

void tabling_destroy(prolog *pl)
{
	for (unsigned i = 0; i < MAX_THREADS; i++)
		tabling_destroy_thread(&pl->threads[i]);
}

// abolish_table/1: drop every variant of ONE predicate. Without this the
// only way to invalidate a table after assert/retract is
// abolish_all_tables/0, which throws away unrelated work too.
//
// The trie node stays; resetting its value to NULL just makes the next
// call see a fresh variant. Nodes are cheap and the same call is likely
// to come back.

static bool bif_tbl_abolish_1(query *q)
{
	tbl_state *s = tbl(q);
	CHECKED(s);

	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,integer);

	// Same rule as abolish_all_tables/0: never while a leader is
	// driving completion or a frame is still enumerating.

	if (s->in_use)
		return throw_error(q, p1, p1_ctx, "permission_error", "modify,table");

	const pl_idx functor = p1->val_off;
	const unsigned arity = (unsigned)get_smallint(p2);
	table *prev = NULL;
	unsigned dropped = 0;

	for (table *t = s->all_tables; t; ) {
		table *next = t->all_next;

		if ((t->functor == functor) && (t->arity == arity)) {
			if (t->leaf)
				t->leaf->value = NULL;

			if (prev)
				prev->all_next = next;
			else
				s->all_tables = next;

			tbl_slot_release(s, t);
			tbl_destroy(t);
			dropped++;
		} else
			prev = t;

		t = next;
	}

	// Only disturb live enumerations if something actually went.

	if (dropped)
		s->generation++;

	return true;
}

static bool bif_tbl_abolish_all_tables_0(query *q)
{
	tbl_state *s = tbl(q);
	CHECKED(s);

	// Freeing tables while a leader is driving completion (or while an
	// enumeration frame is live) would leave dangling handles behind.

	if (s->in_use)
		return throw_error(q, q->st.instr, q->st.cur_ctx, "permission_error", "modify,table");

	s->generation++;
	tbl_clear_all(s);
	return true;
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
	{"$tbl_abolish", 2, bif_tbl_abolish_1, "+atom,+integer", false, false, BLAH},
	{"$tbl_abolish_all_tables", 0, bif_tbl_abolish_all_tables_0, "", false, false, BLAH},


	{0}
};
