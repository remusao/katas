#include <iostream>

int main() {
    int n, v, elt;
    std::cin >> v >> n;
    for (int i = 0; i < n; ++i) {
        std::cin >> elt;
        if (elt == v) {
            std::cout << i << std::endl;
            break;
        }
    }
    return 0;
}
