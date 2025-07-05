#include <stdlib.h>
#include "list.h"

void list_init(list *l)
{
	l->front = l->back = 0;
	l->cnt = 0;
}

void list_push_front(list *l, void *n_)
{
	lnode *n = n_;
    n->prev = 0;

    if ((n->next = l->front) == 0)
        l->back = n;
    else
        l->front->prev = n;

    l->front = n;
    l->cnt++;
}

void list_push_back(list *l, void *n_)
{
	lnode *n = n_;
    n->next = 0;

    if ((n->prev = l->back) == 0)
        l->front = n;
    else
        l->back->next = n;

    l->back = n;
    l->cnt++;
}

void *list_pop_front(list *l)
{
    if (!l->front)
        return 0;

    lnode *n = l->front;
    l->front = l->front->next;

    if (l->front)
        l->front->prev = 0;
    else
        l->back = 0;

    l->cnt--;
    return n;
}

void *list_pop_back(list *l)
{
    if (!l->back)
        return 0;

    lnode *n = l->back;
    l->back = l->back->prev;

    if (l->back)
        l->back->next = 0;
    else
        l->front = 0;

    l->cnt--;
    return n;
}

void *list_remove(list *l, void *n_)
{
	lnode *n = n_;

    if (l->front == n)
        l->front = n->next;
    else
        n->prev->next = n->next;

    if (l->back == n)
        l->back = n->prev;
    else
        n->next->prev = n->prev;

    lnode *save = n->next;
    l->cnt--;
    return save;
}

void listx_init(listx *l)
{
	l->front = l->back = 0;
	l->cnt = 0;
}

void listx_push_front(listx *l, void *entry)
{
	lxnode *n = malloc(sizeof(lxnode));
	n->entry = entry;
    n->prev = 0;

    if ((n->next = l->front) == 0)
        l->back = n;
    else
        l->front->prev = n;

    l->front = n;
    l->cnt++;
}

void listx_push_back(listx *l, void *entry)
{
	lxnode *n = malloc(sizeof(lxnode));
	n->entry = entry;
    n->next = 0;

    if ((n->prev = l->back) == 0)
        l->front = n;
    else
        l->back->next = n;

    l->back = n;
    l->cnt++;
}

void *listx_pop_front(listx *l)
{
    if (!l->front)
        return 0;

    lxnode *n = l->front;
    void *e = n->entry;
    l->front = l->front->next;

    if (l->front)
        l->front->prev = 0;
    else
        l->back = 0;

    l->cnt--;
    free(n);
    return e;
}

void *listx_pop_back(listx *l)
{
    if (!l->back)
        return 0;

    lxnode *n = l->back;
    void *e = n->entry;
    l->back = l->back->prev;

    if (l->back)
        l->back->next = 0;
    else
        l->front = 0;

    l->cnt--;
    free(n);
    return e;
}
