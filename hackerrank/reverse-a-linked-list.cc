/*
  Reverse a linked list and return pointer to the head
  The input list will have at least one element
  Node is defined as
  struct Node
  {
     int data;
     struct Node *next;
  }
*/
Node* Reverse(Node *head)
{
    if (head == nullptr) return nullptr;
    else if (head->next == nullptr) return head;
    else {
        Node* previous = head;
        Node* ptr = head->next;
        while (ptr != nullptr) {
            Node* next = ptr->next;
            ptr->next = previous;
            previous = ptr;
            ptr = next;
        }
        head->next = nullptr;
        return previous;
    }
}
