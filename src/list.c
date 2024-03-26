#include "list.h"

void list_push_front(list *l, void *entry_)
{
	lnode *entry = entry_;
    entry->prev = 0;

    if ((entry->next = l->front) == 0)
        l->back = entry;
    else
        l->front->prev = entry;

    l->front = entry;
    l->cnt++;
}

void list_push_back(list *l, void *entry_)
{
	lnode *entry = entry_;
    entry->next = 0;

    if ((entry->prev = l->back) == 0)
        l->front = entry;
    else
        l->back->next = entry;

    l->back = entry;
    l->cnt++;
}

void *list_pop_front(list *l)
{
    if (!l->front)
        return 0;

    lnode *entry = l->front;
    l->front = l->front->next;

    if (l->front)
        l->front->prev = 0;
    else
        l->back = 0;

    entry->next = 0;
    l->cnt--;
    return entry;
}

void *list_pop_back(list *l)
{
    if (!l->back)
        return 0;

    lnode *entry = l->back;
    l->back = l->back->prev;

    if (l->back)
        l->back->next = 0;
    else
        l->front = 0;

    entry->prev = 0;
    l->cnt--;
    return entry;
}

void *list_remove(list *l, void *entry_)
{
	lnode *entry = entry_;

    if (l->front == entry)
        l->front = entry->next;
    else
        entry->prev->next = entry->next;

    if (l->back == entry)
        l->back = entry->prev;
    else
        entry->next->prev = entry->prev;

    lnode *save = entry->next;
    entry->next = entry->prev = 0;
    l->cnt--;
    return save;
}
