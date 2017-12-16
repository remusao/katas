/*
  Get Nth element from the end in a linked list of integers
  Number of elements in the list will always be greater than N.
  Node is defined as
  struct Node
  {
     int data;
     struct Node *next;
  }
*/
int findRec(Node* head, int& position) {
    if (head == nullptr) return 0;
    else {
        int value = findRec(head->next, position);
        if (position == -1) return value;
        else if (position == 0) {
            position = -1;
            return head->data;
        }
        else {
            --position;
        }
    }

    return 0;
}
int GetNode(Node *head, int positionFromTail)
{
    return findRec(head, positionFromTail);
}
