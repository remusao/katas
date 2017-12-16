Node* Reverse(Node* head)
{
    if (head == nullptr) return nullptr;
    else if (head->next == nullptr) return head;
    else {
        Node *prev, *next;
        Node *ptr = head;
        while (ptr != nullptr) {
            prev = ptr->prev;
            next = ptr->next;
            ptr->prev = next;
            ptr->next = prev;
            ptr = next;
        }
        return prev->prev;
    }
}
