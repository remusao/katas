void decode_huff(node *root, string s)
{
    node* prev = root;
    node* ptr = root;
    for (char c: s) {
        prev = ptr;
        if (c == '1') {
            ptr = ptr->right;
            if (ptr == nullptr) {
                std::cout << prev->data;
                prev = root;
                ptr = root->right;
            }
        }
        else if (c == '0') {
            ptr = ptr->left;
            if (ptr == nullptr) {
                std::cout << prev->data;
                prev = root;
                ptr = root->left;
            }
        }
    }
    std::cout << ptr->data;
}
