#include <iostream>
#include <array>


int main(){
    std::array<int, 1001> counter;
    counter.fill(0);

    // Read values
    int n, value;
    std::cin >> n;
    for (int i = 0; i < n; ++i) {
        std::cin >> value;
        ++counter[value];
    }

    // For each element
    while (n > 0) {
        // Find minimum
        int mini = 1;
        for (int i = 1; i <= 1000; ++i) {
            if (counter[i] > 0) {
                mini = i;
                break;
            }
        }

        std::cout << n << std::endl;
        n -= counter[mini];

        // Substract current cut
        for (int k = mini; k <= 1000; ++k) {
            counter[k - mini] += counter[k];
            counter[k] = 0;
        }
    }
    return 0;
}
