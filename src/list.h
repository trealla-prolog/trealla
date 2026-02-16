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

inline static unsigned long long list_count(list *l) { return l->cnt; }
inline static void *list_front(list *l) { return l->front; }
inline static void *list_back(list *l) { return l->back; }
inline static void *list_prev(void *n) { return ((lnode*)n)->prev; }
inline static void *list_next(void *n) { return ((lnode*)n)->next; }

// This is an external list

typedef struct lxnode_ {
	struct lxnode_ *prev, *next;
	void *entry;
} lxnode;

typedef struct {
	lxnode *front, *back;
	unsigned long long cnt;
} listx;

void listx_init(listx *l);
void listx_push_front(listx *l, void *e);
void listx_push_back(listx *l, void *e);
void *listx_pop_front(listx *l);
void *listx_pop_back(listx *l);

inline static unsigned long long listx_count(listx *l) { return l->cnt; }
