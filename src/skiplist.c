#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <time.h>

#include "skiplist.h"

#ifdef __wasi__
#include "wasm/compat.h"
#endif

typedef struct keyval_ keyval_t;
typedef struct slnode_ slnode_t;

struct keyval_ {
	void *key, *val;
};

#define BUCKET_SIZE 16

struct slnode_ {
	keyval_t bkt[BUCKET_SIZE];
	int nbr;
	slnode_t *forward[];
};

struct sliter_ {
	sliter *next;
	skiplist *l;
	slnode_t *p;
	const void *key;
	int idx;
};

struct skiplist_ {
	slnode_t *header;
	int (*cmpkey)(const void*, const void*, const void*, void *l);
	void (*delkey)(void*, void*, const void*);
	const void *p;
	sliter tmp_iter;
	sliter *iters;
	size_t count;
	int level;
	unsigned seed;
	bool allow_dups, is_tmp_list, wild_card, is_find;
};

#define MAX_LEVELS 16
#define MAX_LEVEL (MAX_LEVELS - 1)

inline static slnode_t *new_node_of_level(unsigned x)
{
	return malloc(sizeof(slnode_t) + ((x+1) * sizeof(slnode_t*)));
}

static int default_cmpkey(const void *p1, const void *p2, __attribute__((unused)) const void *p, void *l)
{
	ptrdiff_t i1 = (ptrdiff_t)p1;
	ptrdiff_t i2 = (ptrdiff_t)p2;
	return i1 < i2 ? -1 : i1 > i2 ? 1 : 0;
}

skiplist *sl_create(int (*cmpkey)(const void*, const void*, const void*, void *), void(*delkey)(void*, void*, const void*), const void *p)
{
	skiplist *l = (skiplist*)calloc(1, sizeof(struct skiplist_));
	if (!l) return NULL;

	l->header = new_node_of_level(MAX_LEVELS);
	if (!l->header) {
		free(l);
		return NULL;
	}

#ifdef NDEBUG
	l->seed = (unsigned)(size_t)(l + clock());
#else
	static unsigned seed = 0xdeadbeef;
	l->seed = ++seed;
#endif

	l->level = 1;

	for (int i = 0; i < MAX_LEVELS; i++)
		l->header->forward[i] = NULL;

	l->header->nbr = 1;
	l->header->bkt[0].key = NULL;
	l->cmpkey = cmpkey ? cmpkey : default_cmpkey;
	l->delkey = delkey;
	l->allow_dups = true;
	l->is_tmp_list = false;
	l->wild_card = false;
	l->p = p;
	return l;
}

void sl_destroy(skiplist *l)
{
	if (!l)
		return;

	slnode_t *p, *q;
	p = l->header;
	q = p->forward[0];
	free(p);
	p = q;

	while (p) {
		q = p->forward[0];

		if (l->delkey) {
			for (int j = 0; j < p->nbr; j++)
				l->delkey(p->bkt[j].key, p->bkt[j].val, l->p);
		}

		free(p);
		p = q;
	}

	while (l->iters) {
		sliter *iter = l->iters;
		l->iters = iter->next;
		free(iter);
	}

	free(l);
}

void sl_wild_card(skiplist *l) { if (l) l->wild_card = true; }
bool sl_is_find(skiplist *l) { return l ? l->is_find : true; }
skiplist *sl_get_map(const sliter *i) { return i->l; }
void sl_allow_dups(skiplist *l, bool mode) { l->allow_dups = mode; }
size_t sl_count(const skiplist *l) { return l ? l->count : 0; }
void sl_set_tmp(skiplist *l) { l->is_tmp_list = true; }

// Modified binary search: return position where it is or ought to be

static int binary_search2(skiplist *l, const keyval_t n[], const void *key, int imax)
{
	int imin = 0, imid = 0;

	while (imax >= imin) {
		imid = (imax + imin) / 2;
		int ok = l->cmpkey(n[imid].key, key, l->p, l);

		if (ok <= 0)
			imin = imid + 1;
		else
			imax = imid - 1;
	}

	int ok = l->cmpkey(n[imid].key, key, l->p, l);

	if (ok <= 0)
		imid++;

	return imid;
}

#ifdef _WIN32
#define rand_r(p1) rand()
#endif

#define frand(seedp) (((double)rand_r(seedp)) / RAND_MAX)

static int random_level(unsigned *seedp)
{
	const double P = 0.5;
	double r = frand(seedp);
	int lvl = (int)(log(r) / log(1.0 - P));
	return lvl < MAX_LEVEL ? lvl : MAX_LEVEL;
}


bool sl_get(skiplist *l, const void *key, const void **val)
{
	int k;
	slnode_t *p, *q = 0;
	p = l->header;

	for (k = l->level - 1; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->cmpkey(q->bkt[q->nbr - 1].key, key, l->p, l) < 0))
			p = q;
	}

	if (!(q = p->forward[0]))
		return false;

	int imid;

	for (imid = 0; imid < q->nbr; imid++) {
		if (l->cmpkey(q->bkt[imid].key, key, l->p, l) == 0)
			break;
	}

	if (imid >= q->nbr)
		return NULL;

	if (val)
		*val = q->bkt[imid].val;

	return true;
}

bool sl_set(skiplist *l, const void *key, const void *val)
{
	slnode_t *update[MAX_LEVELS];
	slnode_t *p, *q;
	slnode_t stash;
	stash.nbr = 0;
	int k;
	p = l->header;

	for (int k = l->level - 1; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->cmpkey(q->bkt[0].key, key, l->p, l) < 0))
			p = q;

		update[k] = p;
	}

	if (p != l->header) {
		int imid = binary_search2(l, p->bkt, key, p->nbr - 1);

		if (p->nbr < BUCKET_SIZE) {
			int j;

			for (j = p->nbr; j > imid; j--)
				p->bkt[j] = p->bkt[j - 1];

			p->bkt[j].key = (void*)key;
			p->bkt[j].val = (void*)val;
			p->nbr++;
			l->count++;
			return true;
		}

		// Don't drop this unless you are 100% sure:

#if 1
		while ((imid < p->nbr) && (l->cmpkey(p->bkt[imid].key, key, l->p, l) == 0))
			imid++;

		if (imid <= BUCKET_SIZE) {
			for (int j = imid; j < p->nbr; j++)
				stash.bkt[stash.nbr++] = p->bkt[j];

			p->nbr = imid;
		}
#endif
	}

	k = random_level(&l->seed);

	if (k >= l->level) {
		l->level++;
		k = l->level - 1;
		update[k] = l->header;
	}

	q = new_node_of_level(k + 1);
	if (!q) return false;

	q->bkt[0].key = (void*)key;
	q->bkt[0].val = (void*)val;
	q->nbr = 1;
	l->count++;

	if (stash.nbr) {
		for (int i = 0; i < stash.nbr; i++, q->nbr++)
			q->bkt[q->nbr] = stash.bkt[i];
	}

	for (; k >= 0; k--) {
		p = update[k];
		q->forward[k] = p->forward[k];
		p->forward[k] = q;
	}

	return true;
}

bool sl_app(skiplist *l, const void *key, const void *val)
{
	slnode_t *update[MAX_LEVELS];
	slnode_t *p, *q;
	slnode_t stash;
	stash.nbr = 0;
	int k;
	p = l->header;

	for (int k = l->level - 1; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->cmpkey(q->bkt[0].key, key, l->p, l) <= 0))
			p = q;

		update[k] = p;
	}

	if (p != l->header) {
		int imid = binary_search2(l, p->bkt, key, p->nbr - 1);

		if (p->nbr < BUCKET_SIZE) {
			int j;

			for (j = p->nbr; j > imid; j--)
				p->bkt[j] = p->bkt[j - 1];

			p->bkt[j].key = (void*)key;
			p->bkt[j].val = (void*)val;
			p->nbr++;
			l->count++;
			return true;
		}

		// Don't drop this unless you are 100% sure:

#if 1
		while ((imid < p->nbr) && (l->cmpkey(p->bkt[imid].key, key, l->p, l) == 0))
			imid++;

		if (imid <= BUCKET_SIZE) {
			for (int j = imid; j < p->nbr; j++)
				stash.bkt[stash.nbr++] = p->bkt[j];

			p->nbr = imid;
		}
#endif
	}

	k = random_level(&l->seed);

	if (k >= l->level) {
		l->level++;
		k = l->level - 1;
		update[k] = l->header;
	}

	q = new_node_of_level(k + 1);
	if (!q) return false;

	q->bkt[0].key = (void*)key;
	q->bkt[0].val = (void*)val;
	q->nbr = 1;
	l->count++;

	if (stash.nbr) {
		for (int i = 0; i < stash.nbr; i++, q->nbr++)
			q->bkt[q->nbr] = stash.bkt[i];
	}

	for (; k >= 0; k--) {
		p = update[k];
		q->forward[k] = p->forward[k];
		p->forward[k] = q;
	}

	return true;
}

bool sl_del(skiplist *l, const void *key)
{
	int k, m;
	slnode_t *update[MAX_LEVELS];
	slnode_t *p, *q;
	p = l->header;

	for (k = l->level - 1; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->cmpkey(q->bkt[q->nbr - 1].key, key, l->p, l) < 0))
			p = q;

		update[k] = p;
	}

	if (!(q = p->forward[0]))
		return false;

	int imid;

	for (imid = 0; imid < q->nbr; imid++) {
		if (l->cmpkey(q->bkt[imid].key, key, l->p, l) == 0)
			break;
	}

	if (imid >= q->nbr)
		return NULL;

	while (imid < (q->nbr - 1)) {
		q->bkt[imid] = q->bkt[imid + 1];
		imid++;
	}

	q->nbr--;
	l->count--;

	if (q->nbr)
		return true;

	m = l->level - 1;

	for (k = 0; k <= m; k++) {
		p = update[k];

		if (!p || (p->forward[k] != q))
			break;

		p->forward[k] = q->forward[k];
	}

	free(q);
	m = l->level - 1;

	while (!l->header->forward[m] && (m > 0))
		m--;

	l->level = m + 1;
	return true;
}

void sl_iterate(const skiplist *l, int (*f)(const void*, const void*, const void*), const void *p1)
{
	slnode_t *p;
	p = l->header;
	p = p->forward[0];

	while (p) {
		slnode_t *q = p->forward[0];

		for (int j = 0; j < p->nbr; j++) {
			if (!f(p->bkt[j].key, p->bkt[j].val, p1))
				return;
		}

		p = q;
	}
}

void sl_find(skiplist *l, const void *key, int (*f)(const void*, const void*, const void*), const void *p1)
{
	slnode_t *p, *q = 0;
	p = l->header;
	l->wild_card = false;

	for (int k = l->level - 1; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->cmpkey(q->bkt[q->nbr - 1].key, key, l->p, l) < 0))
			p = q;
	}

	if (!(q = p->forward[0]))
		return;

	int imid = binary_search2(l, q->bkt, key, q->nbr - 1);
	p = q;

	for (int j = imid; j < p->nbr; j++) {
		if (!f(p->bkt[j].key, p->bkt[j].val, p1))
			return;
	}

	while (p) {
		slnode_t *q = p->forward[0];

		for (int j = 0; j < p->nbr; j++) {
			if (!f(p->bkt[j].key, p->bkt[j].val, p1))
				return;
		}

		p = q;
	}
}

sliter *sl_first(skiplist *l)
{
	sliter *iter;
	l->wild_card = false;

	if (l->is_tmp_list)
		iter = &l->tmp_iter;
	else if (!l->iters) {
		iter = malloc(sizeof(sliter));
		if (!iter) return NULL;
	} else {
		iter = l->iters;
		l->iters = iter->next;
	}

	iter->key = NULL;
	iter->l = l;
	iter->p = l->header->forward[0];
	iter->idx = 0;
	return iter;
}

bool sl_is_next(sliter *iter, void **val)
{
	if (!iter)
		return false;

	while (iter->p) {
		if (iter->idx < iter->p->nbr) {
			if (val)
				*val = iter->p->bkt[iter->idx].val;

			iter->key = iter->p->bkt[iter->idx].key;
			return true;
		}

		iter->p = iter->p->forward[0];
		iter->idx = 0;
	}

	return false;
}

bool sl_next(sliter *iter, void **val)
{
	if (!iter)
		return false;

	while (iter->p) {
		if (iter->idx < iter->p->nbr) {
			if (val)
				*val = iter->p->bkt[iter->idx].val;

			iter->key = iter->p->bkt[iter->idx].key;
			iter->idx++;
			return true;
		}

		iter->p = iter->p->forward[0];
		iter->idx = 0;
	}

	return false;
}

void *sl_key(sliter *i)
{
	if (!i)
		return NULL;

	return (void*)i->key;
}

void sl_remove(skiplist *l, const void *v)
{
	if (!l)
		return;

	slnode_t *p;
	p = l->header;
	p = p->forward[0];

	while (p) {
		slnode_t *q = p->forward[0];

		for (int j = 0; j < p->nbr; j++) {
			if (p->bkt[j].val != v)
				continue;

			while (j < (p->nbr - 1)) {
				p->bkt[j] = p->bkt[j + 1];
				j++;
			}

			p->nbr--;
			l->count--;
			return;
		}

		p = q;
	}
}

sliter *sl_find_key(skiplist *l, const void *key)
{
	slnode_t *p, *q = 0;
	p = l->header;
	l->wild_card = false;
	l->is_find = true;

	for (int k = l->level - 1; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->cmpkey(q->bkt[q->nbr - 1].key, key, l->p, l) < 0))
			p = q;
	}

	if (!(q = p->forward[0]))
		return NULL;

	int imid;

	for (imid = 0; imid < q->nbr; imid++) {
		if (l->cmpkey(q->bkt[imid].key, key, l->p, l) == 0)
			break;
	}

	if (imid >= q->nbr)
		return NULL;

	sliter *iter;

	if (!l->iters) {
		iter = malloc(sizeof(sliter));
		if (!iter) return NULL;
	} else {
		iter = l->iters;
		l->iters = iter->next;
	}

	iter->key = key;
	iter->l = l;
	iter->p = q;
	iter->idx = imid;
	return iter;
}

bool sl_is_next_key(sliter *iter)
{
	if (!iter)
		return false;

	iter->l->is_find = false;

	while (iter->p) {
		while (iter->idx < iter->p->nbr) {
			iter->l->wild_card = false;
			int ok = iter->l->cmpkey(iter->p->bkt[iter->idx].key, iter->key, iter->l->p, iter->l);

			if (!iter->l->wild_card && (ok != 0))
				return false;

			return true;
		}

		iter->p = iter->p->forward[0];
		iter->idx = 0;
	}

	return false;
}

bool sl_next_key(sliter *iter, void **val)
{
	if (!iter)
		return false;

	iter->l->is_find = false;

	while (iter->p) {
		while (iter->idx < iter->p->nbr) {
			iter->l->wild_card = false;
			int ok = iter->l->cmpkey(iter->p->bkt[iter->idx].key, iter->key, iter->l->p, iter->l);

			if (!iter->l->wild_card && (ok != 0))
				return false;

			if (iter->l->wild_card && (ok != 0)) {
				iter->idx++;
				continue;
			}

			if (val)
				*val = iter->p->bkt[iter->idx].val;

			iter->idx++;
			return true;
		}

		iter->p = iter->p->forward[0];
		iter->idx = 0;
	}

	return false;
}

size_t sl_iter_count(const sliter *iter)
{
	return sl_count(iter->l);
}

void sl_done(sliter *iter)
{
	if (!iter)
		return;

	skiplist *l = iter->l;

	if (!l->is_tmp_list) {
		iter->next = l->iters;
		l->iters = iter;
	}

	if (l->is_tmp_list)
		sl_destroy(l);
}

void sl_dump(const skiplist *l, const char *(*f)(const void*, const void*, const void*), const void *p1)
{
	if (!l)
		return;

    const slnode_t *p, *q;
    p = l->header;
    p = p->forward[0];

    while (p) {
		q = p->forward[0];
		printf("%6d: ", p->nbr);

		for (int j = 0; j < p->nbr; j++)
			printf("%s ", f(p->bkt[j].key, p->bkt[j].val, p1));

		printf("\n");
		p = q;
    }

    printf("\n");
}
