#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <time.h>

#include "skiplist.h"
#include "threads.h"

typedef struct slnode_ slnode_t;

struct slnode_ {
	void *key;
	void *val;
	slnode_t *forward[];
};

struct sliter_ {
	sliter *next;
	skiplist *l;
	slnode_t *p;
	void *key;
};

struct skiplist_ {
	slnode_t *header;
	int (*cmpkey)(const void*, const void*, const void*, void *l);
	void (*delkey)(void*, void*, const void*);
	const void *p;
	sliter tmp_iter;
	sliter *iters;
	size_t count;
	lock guard;
	int level;
	unsigned seed;
	bool is_tmp_list, wild_card, is_find, is_destroyed;
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

	init_lock(&l->guard);
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

	l->header->key = NULL;
	l->header->val = NULL;
	l->cmpkey = cmpkey ? cmpkey : default_cmpkey;
	l->delkey = delkey;
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
			l->delkey(p->key, p->val, l->p);
		}

		free(p);
		p = q;
	}

	while (l->iters) {
		sliter *iter = l->iters;
		l->iters = iter->next;
		free(iter);
	}

	l->is_destroyed = true;
	deinit_lock(&l->guard);
	free(l);
}

void sl_set_wild_card(skiplist *l) { if (l) l->wild_card = true; }
bool sl_is_find(skiplist *l) { return l ? l->is_find : true; }
size_t sl_count(const skiplist *l) { return l ? l->count : 0; }
void sl_set_tmp(skiplist *l) { l->is_tmp_list = true; }

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
	if (!l || l->is_destroyed)
		return false;

	slnode_t *p = l->header, *q = NULL;

	for (int k = l->level - 1; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->cmpkey(q->key, key, l->p, l) < 0))
			p = q;
	}

	if (!(q = p->forward[0]))
		return false;

	if (l->cmpkey(q->key, key, l->p, l) != 0)
		return false;

	if (val)
		*val = q->val;

	return true;
}

bool sl_set(skiplist *l, const void *key, const void *val)
{
	if (!l || l->is_destroyed)
		return false;

	slnode_t *update[MAX_LEVELS], *p = l->header, *q = NULL;
	int k;

	for (k = l->level - 1; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->cmpkey(q->key, key, l->p, l) <= 0))
			p = q;

		update[k] = p;
	}

	k = random_level(&l->seed);

	if (k >= l->level) {
		l->level++;
		k = l->level - 1;
		update[k] = l->header;
	}

	q = new_node_of_level(k + 1);
	q->key = (void *)key;
	q->val = (void*)val;

	for (; k >= 0; k--) {
		p = update[k];
		q->forward[k] = p->forward[k];
		p->forward[k] = q;
	}

	l->count++;
	return true;
}

bool sl_rem(skiplist *l, const void *key, const void *val)
{
	if (!l || l->is_destroyed)
		return false;

	slnode_t *update[MAX_LEVELS], *p = l->header, *q = NULL;
	int k;

	for (k = l->level; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->cmpkey(q->key, key, l->p, l) < 0))
			p = q;

		update[k] = p;
	}

	if (!(q = p->forward[0]))
		return false;

	if (l->cmpkey(q->key, key, l->p, l) != 0)
		return false;

	if (q->val != val)
		return false;

	if (l->delkey)
		l->delkey(q->key, q->val, l->p);

	int m = l->level - 1;

	for (k = 0; k <= m; k++) {
		p = update[k];

		if (!p || (p->forward[k] != q))
			break;

		p->forward[k] = q->forward[k];
	}

	m = l->level - 1;

	while (!l->header->forward[m] && (m > 0))
		m--;

	l->level = m + 1;
	l->count--;
	free(q);
	return true;
}

bool sl_del(skiplist *l, const void *key)
{
	if (!l || l->is_destroyed)
		return false;

	slnode_t *update[MAX_LEVELS], *p = l->header, *q = NULL;
	int k;

	for (k = l->level; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->cmpkey(q->key, key, l->p, l) < 0))
			p = q;

		update[k] = p;
	}

	if (!(q = p->forward[0]))
		return false;

	if (l->cmpkey(q->key, key, l->p, l) != 0)
		return false;

	if (l->delkey)
		l->delkey(q->key, q->val, l->p);

	int m = l->level - 1;

	for (k = 0; k <= m; k++) {
		p = update[k];

		if (!p || (p->forward[k] != q))
			break;

		p->forward[k] = q->forward[k];
	}

	m = l->level - 1;

	while (!l->header->forward[m] && (m > 0))
		m--;

	l->level = m + 1;
	l->count--;
	free(q);
	return true;
}

sliter *sl_first(skiplist *l)
{
	if (!l || l->is_destroyed)
		return NULL;

	sliter *iter;
	l->wild_card = false;

	if (l->is_tmp_list)
		iter = &l->tmp_iter;
	else if (!l->iters) {
		iter = malloc(sizeof(sliter));
		if (!iter) return NULL;
	} else {
		acquire_lock(&l->guard);
		iter = l->iters;
		l->iters = iter->next;
		release_lock(&l->guard);
	}

	iter->key = NULL;
	iter->l = l;
	iter->p = l->header->forward[0];
	return iter;
}

bool sl_is_next(sliter *iter, void **val)
{
	if (!iter)
		return false;

	if (!iter->p)
		return false;

	if (val)
		*val = iter->p->val;

	iter->key = iter->p->key;
	return true;
}

bool sl_next(sliter *iter, void **val)
{
	if (!iter)
		return false;

	if (!iter->p)
		return false;

	if (val)
		*val = iter->p->val;

	iter->key = iter->p->key;
	iter->p = iter->p->forward[0];
	return true;
}

void *sl_key(sliter *iter)
{
	if (!iter)
		return NULL;

	return (void*)iter->key;
}

sliter *sl_find_key(skiplist *l, const void *key)
{
	if (!l || l->is_destroyed)
		return NULL;

	slnode_t *p, *q = 0;
	p = l->header;
	l->wild_card = false;
	l->is_find = true;

	for (int k = l->level - 1; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->cmpkey(q->key, key, l->p, l) < 0))
			p = q;
	}

	if (!(q = p->forward[0]))
		return false;

	sliter *iter;

	if (l->is_tmp_list)
		iter = &l->tmp_iter;
	else if (!l->iters) {
		iter = malloc(sizeof(sliter));
		if (!iter) return NULL;
	} else {
		acquire_lock(&l->guard);
		iter = l->iters;
		l->iters = iter->next;
		release_lock(&l->guard);
	}

	iter->key = (void*)key;
	iter->l = l;
	iter->p = q;
	return iter;
}

bool sl_next_key(sliter *iter, void **val)
{
	if (!iter)
		return false;

	iter->l->is_find = false;

	if (!iter->p)
		return false;

	iter->l->wild_card = false;
	int ok = iter->l->cmpkey(iter->p->key, iter->key, iter->l->p, iter->l);

	if (!iter->l->wild_card && (ok != 0))
		return false;

	if (val)
		*val = (void*)iter->p->val;

	iter->p = iter->p->forward[0];
	return true;
}

void sl_done(sliter *iter)
{
	if (!iter)
		return;

	skiplist *l = iter->l;

	if (!l->is_tmp_list) {
		acquire_lock(&l->guard);
		iter->next = l->iters;
		l->iters = iter;
		release_lock(&l->guard);
	}

	if (l->is_tmp_list)
		sl_destroy(l);
}

void sl_dump(const skiplist *l, const char *(*f)(const void*, const void*, const void*), const void *p1)
{
	if (!l || l->is_destroyed)
		return;

	const slnode_t *p, *q;
	p = l->header;
	p = p->forward[0];

	while (p) {
		q = p->forward[0];
		printf("%s ", f(p->key, p->val, p1));
		printf("\n");
		p = q;
	}

	printf("\n");
}
