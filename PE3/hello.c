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
void add_to_front(void* data, list ls)
{
    node* n = (node*)calloc(1, sizeof(node));   // allocate memory for the node
    
    n->data = data;     // set the new node's data member to the data argument
    n->next = ls.head;  // point the new node forward to the old head
    n->prev = NULL;     // point the new node backward to NULL
    
    ls.head->prev = n;  // point the old head backward to new node
    ls.head = n;        // set list's head pointer to the new node
}

/* */
int main()
{
    list* ls = (list*)calloc(1, sizeof(list));
    node* head = (node*)calloc(1, sizeof(node));
    node* tail = (node*)calloc(1, sizeof(node));
    
    head->data = (int*)1;
    head->prev = NULL;
    head->next = tail;

    tail->data = (int*)2;
    tail->prev = head;
    tail->next = NULL;

    ls->head = head;
    ls->tail = tail;

    add_to_front((int*)3, *ls);

    printf("%i, %i", ls->head->data, ls->tail->data);
    return(0);
}