#include <iostream>

/*
  Print elements of a linked list on console
  head pointer input could be NULL as well for empty list
  Node is defined as
  struct Node
  {
     int data;
     struct Node *next;
  }
*/
void Print(Node *head)
{
    while (head != nullptr) {
        std::cout << head->data << std::endl;
        head = head->next;
    }
}
