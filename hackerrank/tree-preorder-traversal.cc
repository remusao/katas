#include <iostream>


void Preorder(node *root) {
    if (root != nullptr) {
        std::cout << (root->data) << ' ';
        Preorder(root->left);
        Preorder(root->right);
    }
}
