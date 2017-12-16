int height(node * root)
{
    if (root == nullptr) return 0;
    else {
        return 1 + max(height(root->left), height(root->right));
    }
}
