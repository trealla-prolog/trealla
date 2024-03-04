#ifndef LIST_H
#define LIST_H

typedef struct lnode_ {
    struct lnode_ *prev, *next;
} lnode;

typedef struct {
    lnode *front, *back;
    unsigned cnt;
} list;

#define list_init(l) { (l)->cnt = 0; (l)->front = (l)->back = NULL; }
#define list_count(l) (l)->cnt
#define list_front(l) (l)->front
#define list_back(l) (l)->back
#define list_prev(n) (n)->prev
#define list_next(n) (n)->next

extern void list_push_front(list *l, lnode *n);
extern void list_push_back(list *l, lnode *n);
extern void list_insert_before(list *l, lnode *n, lnode *v);
extern void list_insert_after(list *l, lnode *n, lnode *v);
extern void list_replace(list *l, lnode *n, lnode *m);
extern void list_concat(list *l, list *from);

// After the following next/prev is not available...

extern lnode *list_remove(list *l, lnode *n);  // return orig n->next
extern lnode *list_pop_front(list *l);
extern lnode *list_pop_back(list *l);

#endif
