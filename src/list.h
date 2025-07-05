#pragma once

// This is an intrusive list & as such the *lnode*
// header must be used as the first field of any struct.

typedef struct lnode_ {
	struct lnode_ *prev, *next;
} lnode;

typedef struct {
	lnode *front, *back;
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
void listx_push_front(listx *l, void *n);
void listx_push_back(listx *l, void *n);
void *listx_pop_front(listx *l);
void *listx_pop_back(listx *l);

inline static unsigned long long listx_count(listx *l) { return l->cnt; }
