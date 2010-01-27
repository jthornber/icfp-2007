#include "list.h"

#include <stdlib.h>

/*----------------------------------------------------------------*/

void list_init(struct list_head *l)
{
        l->next = l->prev = l;
}

inline void list_add(struct list_head *item, struct list_head *p, struct list_head *n)
{
        item->prev = p;
        item->next = n;
        p->next = item;
        n->prev = item;
}

void list_add_head(struct list_head *item, struct list_head *l)
{
        list_add(item, l, l->next);
}

void list_add_tail(struct list_head *item, struct list_head *l)
{
        list_add(item, l->prev, l);
}

void list_merge(struct list_head *front, struct list_head *back)
{
        /* FIXME: check this */
        back->prev->next = front;
        back->next->prev = front->prev;

        front->prev->next = back->next;
        front->prev = back->prev;


        list_init(back);
}

void list_del(struct list_head *item)
{
        item->prev->next = item->next;
        item->next->prev = item->prev;
}

int list_empty(struct list_head *l)
{
        return l->next == l;
}

/*----------------------------------------------------------------*/
