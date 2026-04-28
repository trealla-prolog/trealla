#pragma once

#include <inttypes.h>

// This is an intrusive list & as such the *lnode*
// header must be used as the first field of any struct.

#if (__STDC_VERSION__ >= 201112L) && USE_THREADS
#include <stdatomic.h>
#define list_atomic _Atomic
#else
#define list_atomic volatile
#endif

typedef list_atomic int64_t list_refcnt;

typedef struct lnode_ {
	struct lnode_ *prev, *next;
} lnode;

typedef struct {
	lnode *front, *back;
	list_refcnt cnt;
} list;

void list_init(list *l);
void list_push_front(list *l, void *new);
void list_push_back(list *l, void *new);
void list_insert_after(list *l, void *old, void *new);
void *list_remove(list *l, void *old);
void *list_pop_front(list *l);
void *list_pop_back(list *l);

unsigned long long list_count(list *l);
void *list_front(list *l);
void *list_back(list *l);
void *list_prev(void *n);
void *list_next(void *n);

