Node* SortedInsert(Node *head,int data)
{
    if (head == nullptr) {
        return new Node{data, nullptr, nullptr};
    }
    else if (head->data >= data) {
        // Insert in first position
        return new Node{data, head, nullptr};
    }
    else {
        // Insert after first position
        Node* prev = head;
        Node* ptr = head->next;
        while (ptr != nullptr && ptr->data < data) {
            prev = ptr;
            ptr = ptr->next;
        }

        if (ptr == nullptr) {
            // Insert in last position
            Node* new_node = new Node{data, nullptr, prev};
            prev->next = new_node;
            return head;
        }
        else {
            // Insert before ptr, but not in first position
            Node* new_node = new Node{data, ptr, prev};
            prev->next = new_node;
            ptr->prev = new_node;
            return head;
        }
    }
}
