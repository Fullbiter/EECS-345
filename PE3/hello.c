#include <stdio.h>
#include <stdlib.h>

/* type node */
typedef struct node
{
    void* data;         // data member
    struct node* next;  // pointer to the next node (forward)
    struct node* prev;  // pointer to the previous node (backward)
} node;

/* type list */
typedef struct list
{
    node* head; // pointer to the first node (head)
    node* tail; // pointer to the last node (tail)
} list;

/* */
void add_to_front(void* data, list* ls)
{
    node* n = (node*)calloc(1, sizeof(node));   // allocate memory for the node
    
    n->data = data;         // set the new node's data member to the data argument
    n->next = ls->head;     // point the new node forward to the old head
    n->prev = NULL;         // point the new node backward to NULL
    if (!ls->head)
        ls->tail = n;       // set list's tail pointer to the new node
    else
        ls->head->prev = n; // point the old head backward to the new node
    ls->head = n;           // set list's head pointer to the new node
}

/* */
void add_to_back(void* data, list* ls)
{
    node* n = (node*)calloc(1, sizeof(node));   // allocate memory for the node
    
    n->data = data;         // set the new node's data member to the data argument
    n->prev = ls->tail;     // point the new node backward to the old tail
    n->next = NULL;         // point the new node forward to NULL
    if (!ls->head)
        ls->head = n;       // set list's head pointer to the new node
    else
        ls->tail->next = n; // point the old tail forward to the new node
    ls->tail = n;           // set list's tail pointer to the new node
}

void* remove_from_front(list* ls)
{
    void* data = ls->head->data; // cache the data of the node to remove
    free(ls->head);              // free the memory used by the removed node
    ls->head = ls->head->next;   // set list's head pointer to the next node
    return data;
}

void* remove_from_back(list* ls)
{
    void* data = ls->tail->data; // cache the data of the node to remove
    free(ls->tail);              // free the memory used by the removed node
    ls->tail = ls->tail->prev;   // set list's tail pointer to the prev node
    return data;
}

/* */
int main()
{
    list* ls = (list*)calloc(1, sizeof(list));
    node* head = (node*)calloc(1, sizeof(node));
    node* tail = (node*)calloc(1, sizeof(node));
    
    add_to_front((int*)1, ls);
    add_to_front((int*)2, ls);
    add_to_front((int*)3, ls);

    printf("%i, %i, %i\n", ls->head->data, ls->head->next->data, ls->head->next->next->data);
    printf("Removed %i\n", remove_from_back(ls));
    printf("%i, %i\n", ls->head->data, ls->head->next->data);
    printf("Removed %i\n", remove_from_back(ls));
    printf("%i\n", ls->head->data);
    printf("Removed %i\n", remove_from_back(ls));

    return(0);
}