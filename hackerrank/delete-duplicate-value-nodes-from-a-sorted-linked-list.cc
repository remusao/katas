Node* RemoveDuplicates(Node *head)
{
    Node* ptr = head;
    while (ptr != nullptr) {
        int value = ptr->data;
        while (ptr->next != nullptr && ptr->next->data == value) {
            // Skipe one node
            ptr->next = ptr->next->next;
        }
        ptr = ptr->next;
    }
    return head;
}
