/*
  Delete Node at a given position in a linked list
  Node is defined as
  struct Node
  {
     int data;
     struct Node *next;
  }
*/
Node* Delete(Node *head, int position)
{
    if (head == nullptr) {
        return nullptr;
    }
    else if (position == 0) {
        return head->next;
    }
    else {
        Node* ptr = head;
        for (int i = 0; ptr->next != nullptr && i < (position - 1); ++i) {
            ptr = ptr->next;
        }

        if (ptr->next != nullptr) {
            Node* to_delete = ptr->next;
            ptr->next = to_delete->next;
            delete to_delete;
        }

        return head;
    }
}
