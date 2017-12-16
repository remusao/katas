void LevelOrder(node * root)
{
    if (root == nullptr) return;
    else {
        std::queue<node*> queue;
        queue.push(root);

        while (!queue.empty()) {
            node* elt = queue.front();
            queue.pop();
            if (elt != nullptr) {
                std::cout << elt->data << ' ';
                queue.push(elt->left);
                queue.push(elt->right);
            }
        }
    }
}
