#include <vector>
#include <iostream>

struct Node {
    int value;
    Node* left;
    Node* right;
};


void display(Node* root) {
    if (root != nullptr) {
        display(root->left);
        std::cout << root->value << ' ';
        display(root->right);
    }
}


void recursive_swaps(Node* root, int k, int height) {
    if (root != nullptr) {
        recursive_swaps(root->left, k, height + 1);
        recursive_swaps(root->right, k, height + 1);
        // Swap if `height` is a multiple of `k`
        if (height % k == 0) {
            Node* tmp = root->left;
            root->left = root->right;
            root->right = tmp;
        }
    }
}


int main() {
    int N, T, K;
    int left, right;

    // Read tree
    std::cin >> N;
    std::vector<Node*> nodes(N + 1, nullptr);

    for (int i = 1; i <= N; ++i) {
        nodes[i] = new Node{i, nullptr, nullptr};
    }

    for (int i = 1; i <= N; ++i) {
        std::cin >> left >> right;
        nodes[i]->left = left != -1 ? nodes[left]: nullptr;
        nodes[i]->right = right != -1 ? nodes[right]: nullptr;
    }

    Node* root = nodes[1];

    // Perform swaps
    std::cin >> T;
    while (T--) {
        std::cin >> K;
        recursive_swaps(root, K, 1);
        display(root);
        std::cout << std::endl;
    }
    return 0;
}
