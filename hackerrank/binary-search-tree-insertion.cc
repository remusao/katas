node * insert(node * root, int value)
{
    if (root == nullptr) {
        return new node{value, nullptr, nullptr};
    }
    else {
        if (value <= root->data) {
            if (root->left == nullptr) {
                root->left = insert(nullptr, value);
            }
            else {
                insert(root->left, value);
            }
        }
        else {
            if (root->right == nullptr) {
                root->right = insert(nullptr, value);
            }
            else {
                insert(root->right, value);
            }
        }
    }

    return root;
}
