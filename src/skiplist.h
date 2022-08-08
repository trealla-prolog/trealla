#pragma once

#include <string.h>
#include <stdbool.h>

typedef struct skiplist_ skiplist;
typedef struct sliter_ sliter;

extern skiplist *sl_create(
	int (*cmpkey)(const void *k1, const void *k2, const void *p, void *l),
	void (*delkey)(void *k, void *v, const void* p),
	const void *p
	);

extern void sl_set_tmp(skiplist *l);
extern void sl_allow_dups(skiplist *l, bool mode);
extern bool sl_set(skiplist *l, const void *k, const void *v);
extern bool sl_app(skiplist *l, const void *k, const void *v);
extern bool sl_get(skiplist *l, const void *k, const void **v);
extern bool sl_del(skiplist *l, const void *k);

extern skiplist *sl_get_map(const sliter *i);
extern void sl_wild_card(skiplist *l);
extern bool sl_is_find(skiplist *l);

extern void sl_iterate(
	const skiplist *l,
	int (*callback)(const void *k, const void *v, const void *p),
	const void *p
	);

extern void sl_find(
	skiplist *l,
	const void *k,
	int (*callback)(const void *k, const void *v, const void *p),
	const void *p
	);

extern sliter *sl_find_key(skiplist *l, const void *k);
extern bool sl_is_next_key(sliter *i);
extern bool sl_next_key(sliter *i, void **v);

extern void sl_remove(skiplist *l, const void *v);

extern sliter *sl_first(skiplist *l);
extern bool sl_is_next(sliter *i, void **v);
extern bool sl_next(sliter *i, void **v);

extern size_t sl_iter_count(const sliter *i);
extern void sl_done(sliter *i);

extern size_t sl_count(const skiplist *l);

extern void sl_dump(
	const skiplist *l,
	const char *(*f)(const void* k, const void* v, const void *p),
	const void *p
	);

extern void sl_destroy(skiplist *l);
