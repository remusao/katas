#include <iostream>


int main(){
    int t, n, c, m;
    std::cin >> t;

    while (t--) {
        std::cin >> n >> c >> m;
        int chocolates = n / c;

        // Compute number of bonus wrappers
        int bonus = 0;
        int remaining = chocolates;
        while ((remaining / m) > 0) {
            chocolates += bonus;
            bonus = remaining / m;
            remaining = bonus + (remaining % m);
        }
        chocolates += bonus;
        std::cout << chocolates << std::endl;
    }

    return 0;
}
