#include <iostream>
#include <vector>
#include <list>


struct Node {
    std::list<int> nexts;
};


int cut_tree(const std::vector<Node>& graph, int index, int& size) {
    int cuts = 0;
    size = 1;
    for (int next: graph[index].nexts) {
        // Recursive call to cut subtree if possible
        int sub_size = 0;
        cuts += cut_tree(graph, next, sub_size);

        // If number of node in subtree is even, cut it
        if (sub_size % 2 == 0 && sub_size > 0) ++cuts;
        // Else keep its nodes
        else size += sub_size;
    }

    return cuts;
}


int main() {
    size_t n, m;
    int x, y;
    std::cin >> n >> m;

    // Read tree
    std::vector<Node> graph{n};
    while (m--) {
        std::cin >> x >> y;
        graph[y - 1].nexts.push_back(x - 1);
    }

    // Cut tree into a forest
    int max_cuts = 0;
    for (int i = 0; i < n; ++i) {
        int size = 0;
        int cuts = cut_tree(graph, i, size);
        if (cuts > max_cuts) max_cuts = cuts;
    }
    std::cout << max_cuts << std::endl;
    return 0;
}
