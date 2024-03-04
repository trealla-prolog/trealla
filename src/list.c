#include <stdlib.h>

#ifdef DEBUG
#include <assert.h>
#endif

#include "list.h"

void list_push_front(list *l, lnode *entry)
{
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

void list_push_back(list *l, lnode *entry)
{
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

lnode *list_pop_front(list *l)
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

lnode *list_pop_back(list *l)
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

lnode *list_remove(list *l, lnode *entry)
{
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

void list_replace(list *l, lnode *entry, lnode *entry2)
{
#ifdef DEBUG
    assert(l && entry && entry2);
#endif

    if (l->front == entry)
        l->front = entry2;
    else
        entry->prev->next = entry2;

    if (l->back == entry)
        l->back = entry2;
    else
        entry->next->prev = entry2;

    entry2->prev = entry->prev;
    entry2->next = entry->next;
}

void list_insert_before(list *l, lnode *e, lnode *entry)
{
#ifdef DEBUG
    assert(l && e && entry);
#endif

    if (e->prev)
        e->prev->next = entry;

    entry->prev = e->prev;
    entry->next = e;
    e->prev = entry;

    if (l->front == e)
        l->front = entry;

    l->cnt++;
}

void list_insert_after(list *l, lnode *e, lnode *entry)
{
#ifdef DEBUG
    assert(l && e && entry);
#endif

    if (e->next)
        e->next->prev = entry;

    entry->next = e->next;
    entry->prev = e;
    e->next = entry;

    if (l->back == e)
        l->back = entry;

    l->cnt++;
}

void list_concat(list *l, list *l2)
{
#ifdef DEBUG
    assert(l && l2);
#endif

    if (!l2->front)
        return;

    if (!l->front) {
        l->front = l2->front;
        l->back = l2->back;
        l->cnt = l2->cnt;
    }
    else {
        l->back->next = l2->front;
        l2->front->prev = l->back;
        l->back = l2->back;
        l->cnt += l2->cnt;
    }

    l2->front = l2->back = NULL;
    l2->cnt = 0;
}
