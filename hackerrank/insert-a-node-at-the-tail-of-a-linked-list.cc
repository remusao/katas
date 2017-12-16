/*
  Insert Node at the end of a linked list
  head pointer input could be NULL as well for empty list
  Node is defined as
  struct Node
  {
     int data;
     struct Node *next;
  }
*/
Node* Insert(Node *head, int data)
{
    Node* new_node = new Node{data, nullptr};

    if (head != nullptr) {
        Node* tmp = head;
        while (tmp->next != nullptr) tmp = tmp->next;
        tmp->next = new_node;
    }
    else {
        head = new_node;
    }

    return head;
}
