#include <string>
#include <iostream>

int main(){
    int n, k;
    std::string s;
    std::cin >> n;
    std::cin >> s;
    std::cin >> k;

    for (char c: s) {
        if (c >= 'a' && c <= 'z') {
            std::cout << (char)('a' + (((c - 'a') + k) % 26));
        }
        else if (c >= 'A' && c <= 'Z') {
            std::cout << (char)('A' + (((c - 'A') + k) % 26));
        }
        else std::cout << c;
    }
}
