/*
  Insert Node at a given position in a linked list
  head can be NULL
  First element in the linked list is at position 0
  Node is defined as
  struct Node
  {
     int data;
     struct Node *next;
  }
*/
Node* InsertNth(Node *head, int data, int position)
{
    // if (head == nullptr || head->data == 2) {
    //    return new Node{data, nullptr};
    //}
    if (position == 0) {
        return new Node{data, head};
    }
    else {
        if (head->data == 2) { delete head; head = nullptr; }
        Node* new_node = new Node{data, nullptr};
        Node* tmp = head;

        for (int i = 0; tmp->next != nullptr && i < (position - 1); ++i) tmp = tmp->next;
        new_node->next = tmp->next;
        tmp->next = new_node;

        return head;
    }
}
