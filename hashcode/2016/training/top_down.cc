#include <iostream>
#include <vector>
#include <string>
#include <queue>
#include <ctime>
#include <cstdlib>


struct Tile {
    int r1;
    int c1;
    int r2;
    int c2;
};


// Type of instruction
enum class Instr {
    PAINT_SQUARE,
    PAINT_LINE,
    ERASE_CELL,
    NOPE
};
struct Cmd {
    Instr instr;
    int r1;
    int c1;
    int r2;
    int c2;
    int s;
};


int area(Cmd cmd) {
    if (cmd.instr == Instr::PAINT_SQUARE) {
        int size = 2 * cmd.s + 1;
        return size * size;
    }
    else if (cmd.instr == Instr::PAINT_LINE) {
        if (cmd.r1 == cmd.r2) return (abs(cmd.c2 - cmd.c1) + 1);
        else return (abs(cmd.r2 - cmd.r1) + 1);
    }
    else return 0;
}

inline Cmd paint_square(int r, int c, int s) {
    return Cmd{
        Instr::PAINT_SQUARE,
        r,
        c,
        -1,
        -1,
        s
    };
}

inline Cmd paint_line(int r1, int c1, int r2, int c2) {
    return Cmd{
        Instr::PAINT_LINE,
        r1,
        c1,
        r2,
        c2,
        -1
    };
}

inline Cmd erase_cell(int r, int c) {
    return Cmd{
        Instr::ERASE_CELL,
        r,
        c,
        -1,
        -1,
        -1
    };
}

inline Cmd nope() {
    return Cmd{
        Instr::NOPE,
            -1,
            -1,
            -1,
            -1,
            -1
    };
}


// Type of solution
class Solution {
    public:
        std::vector<Cmd> commands;
};


class Painting {
    public:
        Painting(int n, int m)
            : n(n)
            , m(m)
        {
            // Find surrounding box
            grid_ = new bool*[n];
            for (int i = 0; i < n; ++i) {
                grid_[i] = new bool[m];
            }
            this->reset_grid();
        }

        ~Painting() {
            for (int i = 0; i < this->n; ++i) {
                delete[] grid_[i];
            }
            delete[] grid_;
        }

        void execute_cmd(const Cmd& cmd) {
            if (cmd.instr == Instr::PAINT_SQUARE) {
                if (cmd.s == 0) {
                    this->set(cmd.r1, cmd.c1, true);
                }
                else {
                    int size = cmd.s * 2 + 1;
                    int r = cmd.r1 - cmd.s;
                    int c = cmd.c1 - cmd.s;
                    for (int i = 0; i < size ; ++i) {
                        for (int j = 0; j < size; ++j) {
                            this->set(r + i, c + j, true);
                        }
                    }
                }
            }
            else if (cmd.instr == Instr::PAINT_LINE) {
                if (cmd.c1 == cmd.c2) {
                    // Print column
                    for (int i = cmd.r1; i <= cmd.r2; ++i) {
                        this->set(i, cmd.c1, true);
                    }
                }
                else if (cmd.r1 == cmd.r2) {
                    // Print line
                    for (int i = cmd.c1; i <= cmd.c2; ++i) {
                        this->set(cmd.r1, i, true);
                    }
                }
            }
            else if (cmd.instr == Instr::ERASE_CELL) {
                this->set(cmd.r1, cmd.c1, false);
            }
        }

        void paint_grid(const Solution& solution) {
            this->reset_grid();
            for (const Cmd& cmd: solution.commands) {
                execute_cmd(cmd);
            }
        }

        void reset_grid() {
            for (int i = 0; i < this->n; ++i) {
                for (int j = 0; j < this->m; ++j) {
                    this->set(i, j, false);
                }
            }
        }

        void print_with_area(Tile t) const {
            for (int i = 0; i < n; ++i) {
                for (int j = 0; j < m; ++j) {
                    if (i >= t.r1 && i <= t.r2 && j >= t.c1 && j <= t.c2) {
                        std::cout << "\033[1;37m" << this->get(i, j) << "\033[0m";
                    }
                    else {
                        std::cout << this->get(i, j);
                    }
                }
                std::cout << std::endl;
            }
        }

        void print_grid() const {
            for (int i = 0; i < n; ++i) {
                for (int j = 0; j < m; ++j) {
                    std::cout << this->get(i, j);
                }
                std::cout << std::endl;
            }
        }

        void read_grid() {
            std::string line;
            for (int i = 0; i < n; ++i) {
                std::cin >> line;
                for (int j = 0; j < m; ++j) {
                    this->set(i, j, line[j] == '#');
                }
            }
        }

        Solution get_naive_solution() const {
            Solution solution;
            for (int i = 0; i < n; ++i) {
                for (int j = 0; j < m; ++j) {
                    if (this->get(i, j)) solution.commands.push_back(paint_square(i, j, 0));
                }
            }
            return std::move(solution);
        }

        inline bool get(int i, int j) const { return grid_[i][j]; }
        inline void set(int i, int j, bool value) { grid_[i][j] = value; }

        int n;
        int m;
    private:
        bool** grid_;
};


inline void print_cmd(const Cmd& cmd) {
    if (cmd.instr == Instr::PAINT_SQUARE) {
        std::cout << "PAINT_SQUARE "
                  << cmd.r1
                  << ' '
                  << cmd.c1
                  << ' '
                  << cmd.s
                  << std::endl;
    }
    else if (cmd.instr == Instr::PAINT_LINE) {
        std::cout << "PAINT_LINE "
                  << cmd.r1
                  << ' '
                  << cmd.c1
                  << ' '
                  << cmd.r2
                  << ' '
                  << cmd.c2
                  << std::endl;
    }
    else if (cmd.instr == Instr::ERASE_CELL) {
        std::cout << "ERASE_CELL "
                  << cmd.r1
                  << ' '
                  << cmd.c1
                  << std::endl;
    }
}

template <typename T>
inline void print_commands(const T& commands) {
    for (const Cmd& cmd: commands) {
        print_cmd(cmd);
    }
}

inline void dump_solution(const Solution& solution) {
    std::cout << solution.commands.size() << std::endl;
    print_commands(solution.commands);
}


//
// Solve
//


Tile tile_from_cmd(const Cmd& cmd) {
    if (cmd.instr == Instr::PAINT_LINE) {
        return {
            cmd.r1,
            cmd.c1,
            cmd.r2,
            cmd.c2
        };
    }
    else if (cmd.instr == Instr::PAINT_SQUARE) {
        return {
            cmd.r1 - cmd.s,
            cmd.c1 - cmd.s,
            cmd.r1 + cmd.s,
            cmd.c1 + cmd.s
        };
    }
    else {
        // Note: should not happen
        return {0, 0, 0, 0};
    }
}


Cmd get_best_hline(const Painting& p, Tile t) {
    // Check best horizontal line
    // For each row, find longest line
    int hline_size = 0;
    int hline_r = 0;
    int hline_c = 0;
    int hline_current_start = 0;
    int hline_current_size = 0;
    for (int r = t.r1; r <= t.r2; ++r) {
        int c = t.c1;
        hline_current_start = -1;
        while (c <= t.c2) {
            if (p.get(r, c)) {
                if (hline_current_start == -1) {
                    hline_current_start = c;
                    hline_current_size = 1;
                }
                else ++hline_current_size;
            }
            else {
                // Update longest subsequence found so far
                if (hline_current_size > hline_size) {
                    hline_c = hline_current_start;
                    hline_r = r;
                    hline_size = hline_current_size;
                }

                hline_current_start = -1;
                hline_current_size = 0;
            }
            ++c;
        }

        // Update longest subsequence found so far
        if (hline_current_size > hline_size) {
            hline_c = hline_current_start;
            hline_r = r;
            hline_size = hline_current_size;
        }
    }

    if (hline_size == 0) return nope();
    // std::cout << "hline " << hline_size << ' ' << hline_r << ' ' << hline_c << std::endl;

    return paint_line(hline_r, hline_c, hline_r, hline_c + hline_size - 1);
}

Cmd get_best_vline(const Painting& p, Tile t) {
    // Check best vertical line
    // For each column, find longest line
    int vline_size = 0;
    int vline_r = 0;
    int vline_c = 0;
    int vline_current_start = 0;
    int vline_current_size = 0;
    for (int c = t.c1; c <= t.c2; ++c) {
        int r = t.r1;
        vline_current_start = -1;

        while (r <= t.r2) {
            if (p.get(r, c)) {
                if (vline_current_start == -1) {
                    vline_current_start = r;
                    vline_current_size = 1;
                }
                else ++vline_current_size;
            }
            else {
                // Update longest subsequence found so far
                if (vline_current_size > vline_size) {
                    vline_c = c;
                    vline_r = vline_current_start;
                    vline_size = vline_current_size;
                }

                vline_current_size = 0;
                vline_current_start = -1;
            }
            ++r;
        }

        // Update longest subsequence found so far
        if (vline_current_size > vline_size) {
            vline_c = c;
            vline_r = vline_current_start;
            vline_size = vline_current_size;
        }
    }

    if (vline_size == 0) return nope();
    // std::cout << "vline " << vline_size << ' ' << vline_r << ' ' << vline_c << std::endl;

    return paint_line(vline_r, vline_c, vline_r + vline_size - 1, vline_c);
}


inline bool is_filled_square(const Painting& p, int s, int r, int c) {
    for (int i = 0; i < s; ++i) {
        for (int j = 0; j < s; ++j) {
            if (!p.get(r + i, c + j)) {
                return false;
            }
        }
    }
    return true;
}

inline std::pair<int, int> find_square(const Painting& p, Tile t, int s) {
    for (int r = t.r1; r <= (t.r2 - s + 1); ++r) {
        for (int c = t.c1; c <= (t.c2 - s + 1); ++c) {
            // Check if we can find a square of size `s` starting at (r, c)
            if (is_filled_square(p, s, r, c)) return {r, c};
        }
    }
    return {-1, -1};
}

Cmd get_best_square(const Painting& p, Tile t) {
    int max_size = std::min(abs(t.c2 - t.c1 + 1), abs(t.r2 - t.r1 + 1));
    for (int s = (max_size - 1) / 2; s >= 0; --s) {
        // Look for squares of size (2s + 1) inside of tile `t`
        auto result = find_square(p, t, s * 2 + 1);
        if (result.first > -1 && result.second > -1) {
            return paint_square(result.first + s, result.second + s, s);
        }
    }

    return nope();
}

Cmd get_best_cmd(const Cmd& c1, const Cmd& c2, const Cmd& c3) {
    int c1_area = area(c1);
    int c2_area = area(c2);
    int c3_area = area(c3);

    if      (c1_area > c2_area && c1_area > c3_area) return c1;
    else if (c2_area > c1_area && c2_area > c3_area) return c2;
    else return c3;
}

std::vector<Tile> split_around(Tile t, const Cmd& cmd) {
    // TODO: adapt tiles depending on concentration of filled cells
    Tile cmd_tile = tile_from_cmd(cmd);
    std::vector<Tile> new_tiles;

    if (rand() % 2 == 0) {
        if (t.r1 < cmd_tile.r1) {
            // Top tile
            new_tiles.push_back({t.r1, cmd_tile.c1, cmd_tile.r1 - 1, cmd_tile.c2});
        }
        if (t.r2 > cmd_tile.r2) {
            // Bottom tile
            new_tiles.push_back({cmd_tile.r2 + 1, cmd_tile.c1, t.r2, cmd_tile.c2});
        }
        if (t.c1 < cmd_tile.c1) {
            // Left tile
            new_tiles.push_back({t.r1, t.c1, t.r2, cmd_tile.c1 - 1});
        }
        if (t.c2 > cmd_tile.c2) {
            // Right tile
            new_tiles.push_back({t.r1, cmd_tile.c2 + 1, t.r2, t.c2});
        }
    }
    else {
        if (t.r1 < cmd_tile.r1) {
            // Top tile
            new_tiles.push_back({t.r1, t.c1, cmd_tile.r1 - 1, t.c2});
        }
        if (t.r2 > cmd_tile.r2) {
            // Bottom tile
            new_tiles.push_back({cmd_tile.r2 + 1, t.c1, t.r2, t.c2});
        }
        if (t.c1 < cmd_tile.c1) {
            // Left tile
            new_tiles.push_back({cmd_tile.r1, t.c1, cmd_tile.r2, cmd_tile.c1 - 1});
        }
        if (t.c2 > cmd_tile.c2) {
            // Right tile
            new_tiles.push_back({cmd_tile.r1, cmd_tile.c2 + 1, cmd_tile.r2, t.c2});
        }
    }


    return std::move(new_tiles);
}

std::pair<std::vector<Tile>, Cmd> make_split(const Painting& p, Tile t) {
    auto sq = get_best_square(p, t);
    // std::cout << "Square " << sq.s << ' ' << sq.r1 << ' ' << sq.c1 << std::endl;
    auto l1 = get_best_vline(p, t);
    auto l2 = get_best_hline(p, t);

    // Select command
    Cmd best_cmd = get_best_cmd(sq, l1, l2);
    // p.print_with_area(tile_from_cmd(best_cmd));

    // Split Tile arround Cmd
    return {split_around(t, best_cmd), best_cmd};
}

inline bool is_empty(const Painting& p, Tile t) {
    // Check if tile area is empty
    for (int i = t.r1; i <= t.r2; ++i) {
        for (int j = t.c1; j <= t.c2; ++j) {
            if (p.get(i, j)) return false;
        }
    }
    return true;
}

Solution solve(const Painting& p) {
    Solution solution;

    // Get bounding square arround painting
    std::queue<Tile> tiles;
    tiles.push({0, 0, p.n - 1, p.m - 1});

    while (tiles.size() > 0) {
        // Take first tile
        Tile t = tiles.front();
        tiles.pop();
        if (is_empty(p, t)) continue;
        p.print_with_area(t);

        // Make best split in tile
        std::pair<std::vector<Tile>, Cmd> split = make_split(p, t);

        // Insert new tiles in `tiles`
        for (Tile tile: split.first) tiles.push(tile);

        // Insert new commands in solution
        Cmd new_cmd = split.second;
        solution.commands.push_back(new_cmd);
        // Add ERASE_CELL if needed
        Tile cmd_tile = tile_from_cmd(new_cmd);
        for (int i = cmd_tile.r1; i <= cmd_tile.r2; ++i) {
            for (int j = cmd_tile.c1; j <= cmd_tile.c2; ++j) {
                if (!p.get(i, j)) solution.commands.push_back(erase_cell(i, j));
            }
        }
    }

    return std::move(solution);
}

int main() {
    srand(time(0));

    int n, m;
    std::cin >> n >> m;
    Painting p{n, m};
    p.read_grid();
    // p.print_grid();

    Solution best = solve(p);
    for (int i = 0; i < 10000; ++i) {
        Solution solution = solve(p);
        if (solution.commands.size() < best.commands.size()) {
            std::cout << solution.commands.size() << std::endl;
            best = std::move(solution);
        }
    }

    // Display solution
    // Painting check{n, m};
    // check.paint_grid(solution);
    // check.print_grid();
    dump_solution(best);
    return 0;
}
