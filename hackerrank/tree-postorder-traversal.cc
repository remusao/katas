#include <iostream>


void Postorder(node *root) {
    if (root != nullptr) {
        Postorder(root->left);
        Postorder(root->right);
        std::cout << (root->data) << ' ';
    }
}
