#ifndef LIST_H
#define LIST_H

/*----------------------------------------------------------------*/

/*
  Circularly linked lists
*/

struct list_head {
        struct list_head *next;
        struct list_head *prev;
};

void list_init(struct list_head *l);
void list_add_head(struct list_head *item, struct list_head *l);
void list_add_tail(struct list_head *item, struct list_head *l);
void list_merge(struct list_head *l1, struct list_head *l2);
void list_del(struct list_head *item);
int list_empty(struct list_head *l);

#define list_for_each(pos, head) \
	for (pos = (head)->next; pos != (head); pos = pos->next)

#define list_for_each_entry(pos, head, member) \
	for (pos = container_of((head)->next, typeof(*pos), member); \
		&pos->member != (head); \
		pos = container_of(pos->member.next, typeof(*pos), member))

#define list_for_each_entry_safe(pos, n, head, member) \
	for (pos = container_of((head)->next, typeof(*pos), member), n = container_of(pos->member.next, typeof(*pos), member); \
		&pos->member != (head); \
		pos = n, n = container_of(pos->member.next, typeof(*n), member))

#define offsetof(TYPE, MEMBER) ((size_t) &((TYPE *)0)->MEMBER)

#define container_of(ptr, type, member) ({			\
        const typeof( ((type *)0)->member ) *__mptr = (ptr);	\
        (type *)( (char *)__mptr - offsetof(type,member) );})

#define list_entry(ptr, type, member) container_of(ptr, type, member)

/*----------------------------------------------------------------*/

#endif
