#pragma once

#include <string.h>
#include <stdbool.h>

typedef struct skiplist_ skiplist;
typedef struct sliter_ sliter;

skiplist *sl_create(
	int (*cmpkey)(const void *k1, const void *k2, const void *p, void *l),
	void (*delkey)(void *k, void *v, const void* p),
	const void *p
	);

bool sl_set(skiplist *l, const void *k, const void *v);
bool sl_app(skiplist *l, const void *k, const void *v);
bool sl_get(skiplist *l, const void *k, const void **v);
bool sl_del(skiplist *l, const void *k);

void sl_set_wild_card(skiplist *l);
void sl_set_tmp(skiplist *l);
bool sl_is_find(skiplist *l);

void sl_iterate(
	const skiplist *l,
	int (*callback)(const void *k, const void *v, const void *p),
	const void *p
	);

void sl_find(
	skiplist *l,
	const void *k,
	int (*callback)(const void *k, const void *v, const void *p),
	const void *p
	);

sliter *sl_find_key(skiplist *l, const void *k);
bool sl_is_next_key(sliter *i);
bool sl_next_key(sliter *i, void **v);

sliter *sl_first(skiplist *l);
bool sl_is_next(sliter *i, void **v);
bool sl_next(sliter *i, void **v);
bool sl_next_mutable(sliter *i, void **v);
void *sl_key(sliter *i);

size_t sl_iter_count(const sliter *i);
size_t sl_count(const skiplist *l);
void sl_done(sliter *i);

void sl_dump(
	const skiplist *l,
	const char *(*f)(const void* k, const void* v, const void *p),
	const void *p
	);

void sl_destroy(skiplist *l);
