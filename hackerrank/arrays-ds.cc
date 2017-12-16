#include <vector>
#include <iostream>


int main() {
    int n, elt;
    std::cin >> n;
    std::vector<int> vec;
    vec.reserve(n);

    while (n--) {
        std::cin >> elt;
        vec.push_back(elt);
    }

    for (int i = vec.size() - 1; i >= 0; --i)
        std::cout << vec[i] << ' ';
    std::cout << std::endl;
    return 0;
}
