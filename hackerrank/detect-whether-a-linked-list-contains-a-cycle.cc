Node* safe_incr(Node* head) {
    if (head != nullptr) return head->next;
    else return nullptr;
}

int HasCycle(Node* head)
{
    if (head == nullptr) return 0;
    else {
        Node* fast = head;
        Node* slow = head;

        do {
            fast = safe_incr(safe_incr(fast));
            slow = safe_incr(slow);
        } while (fast != nullptr && slow != nullptr && slow != fast);

        if (fast != nullptr && slow != nullptr && slow == fast) {
            return 1;
        }
        else {
            return 0;
        }
    }
}
