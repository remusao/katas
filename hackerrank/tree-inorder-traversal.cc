void Inorder(node *root) {
    if (root != nullptr) {
        Inorder(root->left);
        std::cout << (root->data) << ' ';
        Inorder(root->right);
    }
}
