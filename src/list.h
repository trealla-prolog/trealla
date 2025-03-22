#pragma once

// This is an intrusive list & as such the *lnode*
// header must be used as the first field of any struct.

typedef struct lnode_ {
	struct lnode_ *prev, *next;
} lnode;

typedef struct lnode_ lnode;

typedef struct {
	lnode *front, *back;
	unsigned cnt;
} list;

void list_init(list *l);
unsigned list_count(list *l);
void *list_front(list *l);
void *list_back(list *l);
void *list_prev(void *n);
void *list_next(void *n);

void list_push_front(list *l, void *n);
void list_push_back(list *l, void *n);
void *list_remove(list *l, void *n);
void *list_pop_front(list *l);
void *list_pop_back(list *l);
