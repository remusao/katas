/*
  Merge two sorted lists A and B as one linked list
  Node is defined as
  struct Node
  {
     int data;
     struct Node *next;
  }
*/
Node* MergeLists(Node *headA, Node* headB)
{
    if (headA == nullptr) return headB;
    else if (headB == nullptr) return headA;
    else {
        if (headA->data <= headB->data) {
            Node* merged = MergeLists(headA->next, headB);
            headA->next = merged;
            return headA;
        }
        else {
            Node* merged = MergeLists(headA, headB->next);
            headB->next = merged;
            return headB;
        }
    }
}
