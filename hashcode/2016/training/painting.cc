#include <iostream>
#include <vector>
#include <string>
#include <random>

std::random_device rd;
std::mt19937 rng(rd());

// Type of instruction
enum class Instr {PAINT_SQUARE, PAINT_LINE, ERASE_CELL};
struct Cmd {
    Instr instr;
    int r1;
    int c1;
    int r2;
    int c2;
    int s;
};


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


// Type of solution
class DNA {
    public:
        std::vector<Cmd> commands;
};


class Painting {
    public:
        Painting(int n, int m)
            : n(n)
            , m(m)
        {
            grid_ = new bool*[n];
            for (int i = 0; i < n; ++i) {
                grid_[i] = new bool[m];
            }
        }

        ~Painting() {
            for (int i = 0; i < n; ++i) {
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
            else {
                this->set(cmd.r1, cmd.c1, false);
            }
        }

        void paint_grid(const DNA& solution) {
            this->reset_grid();
            for (const Cmd& cmd: solution.commands) {
                execute_cmd(cmd);
            }
        }

        void reset_grid() {
            for (int i = 0; i < n; ++i) {
                for (int j = 0; j < m; ++j) {
                    this->set(i, j, false);
                }
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

        DNA get_naive_solution() const {
            DNA solution;
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


int score(const DNA& solution, const Painting& painting) {
    // Check distance between solution and original painting
    // int distance = 0;
    // Painting result{painting.n, painting.m};
    // result.paint_grid(solution);
    // for (int i = 0; i < painting.n; ++i) {
    //     for (int j = 0; j < painting.m; ++j) {
    //         if (result.get(i, j) != painting.get(i, j)) ++distance;
    //     }
    // }

    // return (size / 100) * 25 + (distance / 100) * 75;
    return solution.commands.size();
}


std::vector<Cmd> split_command(Cmd cmd) {
    std::vector<Cmd> sub_commands;
    if (cmd.instr == Instr::PAINT_LINE) {
        if (cmd.c1 == cmd.c2) {
            // Print column
            for (int i = cmd.r1; i <= cmd.r2; ++i) {
                sub_commands.push_back(paint_square(i, cmd.c1, 0));
            }
        }
        else if (cmd.r1 == cmd.r2) {
            // Print line
            for (int i = cmd.c1; i <= cmd.c2; ++i) {
                sub_commands.push_back(paint_square(cmd.r1, i, 0));
            }
        }
    }
    else if (cmd.instr == Instr::PAINT_SQUARE) {
        if (cmd.s == 0) {
            sub_commands.push_back(cmd);
        }
        else {
            int size = cmd.s * 2 + 1;
            int r = cmd.r1 - cmd.s;
            int c = cmd.c1 - cmd.s;
            for (int i = 0; i < size ; ++i) {
                for (int j = 0; j < size; ++j) {
                    sub_commands.push_back(paint_square(r + i, c + j, 0));
                }
            }
        }
    }
    else {
        sub_commands.push_back(cmd);
    }

    return std::move(sub_commands);
}

std::pair<int, std::vector<Cmd>> random_split(DNA& solution) {
    // Select a random command to split
    int indice = std::uniform_int_distribution<int>{0, solution.commands.size() - 1}(rng);
    Cmd cmd = solution.commands[indice];
    return {indice, split_command(cmd)};
}

std::pair<std::vector<int>, Cmd> random_merge(DNA& solution) {
    // Choose kind of merge
    // 1. to square
    // 2. to vertical line
    // 3. to horizontal line
    int merge_kind = std::uniform_int_distribution<int>{0, 2}(rng);

    // Choose starting point of merge
    int indice = std::uniform_int_distribution<int>{0, solution.commands.size() - 1}(rng);

    // Find list of commands to replace
    return {{}, paint_square(0, 0, 0)};
}

DNA neighbour(DNA& solution) {
    // Clone solution
    DNA new_solution = solution;

    // Try to split
    std::pair<int, std::vector<Cmd>> split = random_split(solution);

    // Try to merge
    std::pair<std::vector<int>, Cmd> merge = random_merge(solution);

    return new_solution;
}

DNA optimize(const Painting& painting) {
    // Run simulated annealing
    int k_max = 1000000;
    int temperature = k_max;
    auto s = painting.get_naive_solution();
    int s_score = score(s, painting);

    for (int k = 0; k < k_max; ++k) {
        // New temperature
        --temperature;

        // Pick random neighbour
        auto n = neighbour(s);
        int n_score = score(n, painting);

        // Check if we should replace existing solution
        if (n_score < s_score) {
            s = n;
            s_score = n_score;
        }
    }

    return s;
}



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
        std::cout << "PAINT_LINE"
                  << cmd.r1
                  << ' '
                  << cmd.c1
                  << ' '
                  << cmd.r2
                  << ' '
                  << cmd.c2
                  << std::endl;
    }
    else {
        std::cout << "ERASE_CELL"
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

inline void dump_solution(const DNA& solution) {
    std::cout << solution.commands.size() << std::endl;
    print_commands(solution.commands);
}

int main() {
    int n, m;
    std::cin >> n >> m;
    Painting p{n, m};
    p.read_grid();
    p.print_grid();
    dump_solution(optimize(p));
    return 0;
}
