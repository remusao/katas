#include <iostream>
#include <vector>
#include <string>


bool GRID[1000][1000];
struct Painting {
    int n;
    int m;
    inline bool get(int i, int j) const { return GRID[i][j]; }
    inline void set(int i, int j, bool value) { GRID[i][j] = value; }
};

void read_grid(Painting& painting) {
    std::string line;
    for (int i = 0; i < painting.n; ++i) {
        std::cin >> line;
        for (int j = 0; j < painting.m; ++j) {
            painting.set(i, j, line[j] == '#');
        }
    }
}

void print_grid(const Painting& painting) {
    for (int i = 0; i < painting.n; ++i) {
        for (int j = 0; j < painting.m; ++j) {
            std::cout << painting.get(i, j);
        }
        std::cout << std::endl;
    }
}

int main() {
    Painting p;
    std::cin >> p.n >> p.m;
    read_grid(p);
    dump_solution(optimize(p));
    return 0;
}
