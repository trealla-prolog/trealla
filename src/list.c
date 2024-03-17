#include <stdlib.h>

#ifdef DEBUG
#include <assert.h>
#endif

#include "list.h"

void list_push_front(list *l, void *entry_)
{
	lnode *entry = entry_;

#ifdef DEBUG
    assert(l && entry);
#endif

    entry->prev = NULL;

    if ((entry->next = l->front) == NULL)
        l->back = entry;
    else
        l->front->prev = entry;

    l->front = entry;
    l->cnt++;
}

void list_push_back(list *l, void *entry_)
{
	lnode *entry = entry_;

#ifdef DEBUG
    assert(l && entry);
#endif

    entry->next = NULL;

    if ((entry->prev = l->back) == NULL)
        l->front = entry;
    else
        l->back->next = entry;

    l->back = entry;
    l->cnt++;
}

void *list_pop_front(list *l)
{
#ifdef DEBUG
    assert(l);
#endif

    if (!l->front)
        return NULL;

    lnode *entry = l->front;
    l->front = l->front->next;

    if (l->front)
        l->front->prev = NULL;
    else
        l->back = NULL;

    entry->next = NULL;
    l->cnt--;
    return entry;
}

void *list_pop_back(list *l)
{
#ifdef DEBUG
    assert(l);
#endif

    if (!l->back)
        return NULL;

    lnode *entry = l->back;
    l->back = l->back->prev;

    if (l->back)
        l->back->next = NULL;
    else
        l->front = NULL;

    entry->prev = NULL;
    l->cnt--;
    return entry;
}

void *list_remove(list *l, void *entry_)
{
	lnode *entry = entry_;

#ifdef DEBUG
    assert(l && entry);
#endif

    if (l->front == entry)
        l->front = entry->next;
    else
        entry->prev->next = entry->next;

    if (l->back == entry)
        l->back = entry->prev;
    else
        entry->next->prev = entry->prev;

    lnode *save = entry->next;
    entry->next = entry->prev = NULL;
    l->cnt--;
    return save;
}
