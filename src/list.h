#pragma once

// This is an intrusive list & as such the *linode*
// header must be used as the first field of any struct.

typedef struct linode_ {
	struct linode_ *prev, *next;
} linode;

typedef struct linode_ linode;

typedef struct {
	linode *front, *back;
	unsigned long long cnt;
} list;

void list_init(list *l);
void list_push_front(list *l, void *n);
void list_push_back(list *l, void *n);
void *list_remove(list *l, void *n);
void *list_pop_front(list *l);
void *list_pop_back(list *l);

inline static unsigned long long list_count(list *l) { return l->cnt; }
inline static void *list_front(list *l) { return l->front; }
inline static void *list_back(list *l) { return l->back; }
inline static void *list_prev(void *n) { return ((linode*)n)->prev; }
inline static void *list_next(void *n) { return ((linode*)n)->next; }
