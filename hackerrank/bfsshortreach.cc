#include <iostream>
#include <vector>
#include <list>
#include <queue>

struct Node {
    std::list<int> nexts;
};


int main() {
    size_t t, n;
    int m, s, x, y;
    std::cin >> t;

    while (t--) {
        std::cin >> n >> m;

        // Create graph
        std::vector<Node> graph{n};
        while (m--) {
            std::cin >> x >> y;
            graph[x - 1].nexts.push_back(y - 1); 
            graph[y - 1].nexts.push_back(x - 1); 
        }

        // Read starting point in graph
        std::cin >> s;
        --s;

        // Find shortest path between nodes 
        std::vector<bool> mask;
        mask.resize(n, false);

        std::vector<int> distances;
        distances.resize(n, -1);

        std::queue<int> q;
        q.push(s);
        q.push(-1);
        mask[s] = true;

        int distance = 0;
        while (!q.empty()) {
            int node = q.front(); q.pop();
            if (node == -1) {
                distance += 6;
                if (!q.empty()) q.push(-1);
                continue;
            }
            else {
                distances[node] = distance;
                for (int next: graph[node].nexts) {
                    if (!mask[next]) {
                        mask[next] = true;
                        q.push(next);
                    }
                }
            }
        }

        // Display result
        for (int i = 0; i < n; ++i) {
            if (i != s) {
                std::cout << distances[i] << ' ';
            }
        }
        std::cout << std::endl;
    }
    return 0;
}
