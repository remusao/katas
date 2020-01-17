#include <iostream>
#include <cstdio>
#include <vector>
#include <string>
#include <algorithm>
#include <cmath>
#include <tuple>
#include <queue>
#include <cassert>


template<typename T, typename Container=std::deque<T>>
class iterable_queue : public std::priority_queue<T, Container>
{
public:
    typedef typename Container::iterator iterator;
    typedef typename Container::const_iterator const_iterator;

    iterator begin() { return this->c.begin(); }
    iterator end() { return this->c.end(); }
    const_iterator begin() const { return this->c.begin(); }
    const_iterator end() const { return this->c.end(); }
};


struct Customer {
    size_t id;
    size_t r;
    size_t c;
    std::vector<size_t> orders;

    // Copy
    Customer(const Customer&) = delete;
    Customer& operator=(const Customer&) = delete;

    // Move
    Customer(Customer&&) = default;
    Customer& operator=(Customer&&) = default;

    inline bool operator<(const Customer& customer) const {
        return this->orders.size() < customer.orders.size();
    }
};


struct Drone {
    size_t id;
    size_t available;
    size_t r;
    size_t c;

    // We use a max heap, and we want drone with lowest availability first
    inline bool operator<(const Drone& d) const {
        return this->available > d.available;
    }
};


struct Warehouse {
    size_t r;
    size_t c;
    std::vector<size_t> products;

    // Copy
    Warehouse(const Warehouse&) = delete;
    Warehouse& operator=(const Warehouse&) = delete;

    // Move
    Warehouse(Warehouse&&) = default;
    Warehouse& operator=(Warehouse&&) = default;
};


struct Instr {
    size_t drone;
    char cmd;     // L[oad], D[eliver], U[nload], W[ait]
    size_t target;   // Warehouse (c == 'L' or c == 'U') or Customer (c == 'D')
    size_t product;  // Product type
    size_t quantity; // Quantity of product or time to wait (c == 'W')
};


struct Simulation {
    // Parameters
    size_t nrows;          // number of rows
    size_t ncols;          // number of columns
    size_t ndrones;        // D
    size_t nturns;         // T
    size_t max_payload;    // maximum load of a drone
    size_t nproducts;      // P
    size_t nwarehouses;    // W
    size_t nclients;       // C

    // Entities of the simulation
    std::vector<size_t> products;
    std::vector<Warehouse> warehouses;
    std::vector<Customer> customers;
    std::vector<std::vector<size_t>> index;
    std::vector<size_t> closest_warehouse; // for each client
    iterable_queue<Drone> drones;
};


void display_index(const Simulation& s) {
    fprintf(stderr, "==Index==\n");
    for (size_t product = 0; product < s.nproducts; ++product) {
        fprintf(stderr, "product: %zu\n", product);
        for (size_t warehouse: s.index[product]) {
            fprintf(stderr, "> warehouse: %zu\n", warehouse);
        }
    }
}


void display_simulation(const Simulation& s) {
    fprintf(stderr, "\n");
    for (size_t r = 0; r < s.nrows; ++r) {
        for (size_t c = 0; c < s.ncols; ++c) {
            bool printed = false;
            // Check if drone
            for (const Drone& d: s.drones) {
                if (r == d.r && c == d.c) {
                    fprintf(stderr, "%zu", d.id);
                    printed = true;
                    break;
                }
            }
            if (printed) continue;

            // Check if warehouse
            for (const Warehouse& w: s.warehouses) {
                if (r == w.r && c == w.c) {
                    fprintf(stderr, "W");
                    printed = true;
                    break;
                }
            }
            if (printed) continue;

            // Check if customer
            for (const Customer& customer: s.customers) {
                if (r == customer.r && c == customer.c) {
                    fprintf(stderr, "C");
                    printed = true;
                    break;
                }
            }
            if (printed) continue;

            fprintf(stderr, "#");
        }
        fprintf(stderr, "\n");
    }
}


inline size_t distance(int r1, int c1, int r2, int c2) {
    size_t a = std::abs(r1 - r2);
    size_t b = std::abs(c1 - c2);
    return std::round(std::sqrt(a * a + b * b));
}


void display_instructions(const std::vector<Instr>& instructions) {
    fprintf(stderr, "%zu\n", instructions.size());
    printf("%zu\n", instructions.size());
    for (const Instr& instr: instructions) {
        if (instr.cmd == 'W') {
            printf("%zu W %zu\n", instr.drone, instr.quantity);
        }
        else {
            printf("%zu %c %zu %zu %zu\n",
                   instr.drone,
                   instr.cmd,
                   instr.target,
                   instr.product,
                   instr.quantity);
        }
    }
}


void take_good(size_t product, Warehouse& w, size_t wi, std::vector<std::vector<size_t>>& index) {
    // Remove `product` from `w`
    assert(w.products[product] > 0);
    --w.products[product];
    // Update index
    if (w.products[product] == 0) {
        std::remove_if(
            index[product].begin(),
            index[product].end(),
            [wi](size_t i) { return i == wi; });
    }
}


inline size_t customer_weight(const std::vector<size_t>& products, const std::vector<size_t>& orders) {
    size_t total = 0;
    for (size_t product: orders) {
        total += products[product];
    }
    return total;
}

inline size_t missing_goods(const Simulation& s, const std::vector<size_t>& orders, const std::vector<size_t>& ws) {
    // TODO: Optimize, this is a very hot spot
    static std::vector<size_t> availability(s.nproducts, 0);
    size_t missing = 0;

    if (ws.size() == 0) {
        for (size_t p: orders) {
            missing += s.products[p];
        }
    }
    else {
        if (ws.size() == 1) {
            const Warehouse& w = s.warehouses[ws[0]];
            for (size_t p = 0; p < s.nproducts; ++p) availability[p] = w.products[p];
        }
        else {
            // Reset array of goods
            for (size_t p = 0; p < s.nproducts; ++p) availability[p] = 0;

            // Accumulate goods for each warehouses
            for (size_t wi: ws) {
                const Warehouse& w = s.warehouses[wi];
                for (size_t p = 0; p < s.nproducts; ++p) {
                    availability[p] += w.products[p];
                }
            }
        }

        // Check each order
        for (size_t p: orders) {
            if (availability[p] <= 0) missing += s.products[p];
            else {
                --availability[p];
            }
        }
    }

    return missing;
}


inline bool has_every_goods(const Simulation& s, const std::vector<size_t>& orders, const std::vector<size_t>& ws) {
    return missing_goods(s, orders, ws) == 0;
}


inline size_t compute_cost(const Simulation& s, size_t r, size_t c, std::vector<size_t>& ws, const Customer& customer) {
    size_t cost = 0;
    for (size_t wi: ws) {
        const Warehouse& w = s.warehouses[wi];
        cost += distance(r, c, w.r, w.c);
        r = w.r;
        c = w.c;
    }
    cost += distance(r, c, customer.r, customer.c);
    return cost;
}


inline size_t find_best_order(const Simulation& s, size_t r, size_t c, std::vector<size_t>& ws, const Customer& customer) {
    // Sort by distance from origin
    std::sort(ws.begin(), ws.end(), [&s, &customer](size_t w1, size_t w2) {
        return distance(customer.r, customer.c, s.warehouses[w1].r, s.warehouses[w1].c) > distance(customer.r, customer.c, s.warehouses[w2].r, s.warehouses[w2].c);
    });
    return compute_cost(s, r, c, ws, customer);
}


template <typename T>
inline size_t closest(const std::vector<T>& elts, size_t r, size_t c) {
    size_t min_dist = 4000000000;
    size_t closest_elt = 0;

    for (size_t i = 0; i < elts.size(); ++i) {
        size_t d = distance(r, c, elts[i].r, elts[i].c);
        if (d < min_dist) {
            min_dist = d;
            closest_elt = i;
        }
    }

    return closest_elt;
}


inline size_t closest_warehouse(const Simulation& s, size_t customer) {
    return s.closest_warehouse[customer];
}


void build_closest_warehouse(Simulation& s) {
    // std::vector<size_t> closest_warehouse; // for each client
    s.closest_warehouse.resize(s.nclients);
    for (size_t i = 0; i < s.nclients; ++i) {
        const Customer& c = s.customers[i];
        s.closest_warehouse[i] = closest(s.warehouses, c.r, c.c);
    }
}

void build_index(Simulation& s) {
    s.index.resize(s.nproducts);

    for (size_t warehouse_index = 0; warehouse_index < s.nwarehouses; ++warehouse_index) {
        for (size_t product = 0; product < s.nproducts; ++product) {
            if (s.warehouses[warehouse_index].products[product] > 0) {
                s.index[product].push_back(warehouse_index);
            }
        }
    }
}




// <new_drone, cost, instructions>
std::tuple<Drone, size_t, std::vector<Instr>> deliver(Simulation& s, Drone d, Customer& c, std::vector<size_t>& ws) {
    // First optimization:
    // ==================
    // Between w1 and w2, check if there is something in w1 (and not needed) which is needed in w2 or w3 or etc.
    // => Take it, and dump it in next warehouse => repeat operation for each pair of warehouses

    // Second optimization:
    // ===================
    // Between last warehouse and client. If there is enough space left in drone, check around customer if another
    // customer would need something that is in last warehouse. If so, take it, then deliver to Customer1
    // + deliver extra package to customer 2

    // We want to go to customer, going through every warehouse in `ws`
    std::vector<Instr> instructions;
    std::vector<size_t> orders(s.nproducts, 0);
    std::vector<size_t> ok_orders(s.nproducts, 0);
    for (size_t product: c.orders) ++orders[product];

    size_t cost = 0;
    size_t row = d.r;
    size_t col = d.c;
    size_t weight = 0;

    // Visit every warehouse and take goods as we go
    for (size_t warehouse_index: ws) {
        // TODO: do not be greedy here. Don't take product in the first warehouse it appears,
        // take it in the warehouse where it is less needed
        Warehouse& w = s.warehouses[warehouse_index];
        for (size_t product = 0; product < s.nproducts; ++product) {
            if (orders[product] > 0 && w.products[product] > 0 && (weight + s.products[product]) <= s.max_payload) {
                // Take as much `product` from warehouse as we can and update index
                size_t can_take = std::min(orders[product], w.products[product]);
                size_t took = 0;
                for (size_t i = 0; i < can_take && (weight + s.products[product]) <= s.max_payload; ++i) {
                    take_good(product, w, warehouse_index, s.index);
                    weight += s.products[product];
                    ++ok_orders[product];
                    --orders[product];
                    ++took;
                }
                ++cost;

                instructions.push_back({
                    d.id,
                    'L',
                    warehouse_index,
                    product,
                    took
                });
            }
        }
        cost += distance(row, col, w.r, w.c);
    }

    // Deliver to client
    for (size_t product = 0; product < s.nproducts; ++product) {
        if (ok_orders[product] > 0) {
            instructions.push_back({
                d.id,
                'D',
                c.id,
                product,
                ok_orders[product]
            });
        }
    }

    // Create remaining orders
    std::vector<size_t> new_orders;
    for (size_t product = 0; product < s.nproducts; ++product) {
        for (size_t i = 0; i < orders[product]; ++i) {
            new_orders.push_back(product);
        }
    }

    // Update drone position
    d.r = c.r;
    d.c = c.c;

    // Update order list of client
    if (new_orders.empty()) {
        // Remove client from list
        s.customers.erase(std::find_if(
            s.customers.begin(),
            s.customers.end(),
            [&c](const Customer& customer) { return c.id == customer.id; }));
    }
    else {
        c.orders = new_orders; //std::move(new_orders);
    }

    return std::make_tuple(d, cost, std::move(instructions));
}


// <cost, warehouses>
std::pair<size_t, std::vector<size_t>> find_minimum_path(const Simulation& s, size_t r, size_t c, const Customer& customer) {
    size_t min_cost = s.nturns + 1;
    std::vector<size_t> best_ws;

    // Rank trajects of 1, 2, 3 warehouses
    for (size_t i = 0; i < s.nwarehouses; ++i) {
        if (has_every_goods(s, customer.orders, {i})) {
            std::vector<size_t> ws = {i};
            size_t cost = find_best_order(s, r, c, ws, customer);
            if (cost < min_cost) {
                min_cost = cost;
                best_ws = ws;
            }
        }
    }

    // Trajects of size 2
    for (size_t i = 0; i < s.nwarehouses; ++i) {
        for (size_t j = 0; j < s.nwarehouses; ++j) {
            std::vector<size_t> ws = {i, j};
            if (i != j && has_every_goods(s, customer.orders, ws)) {
                size_t cost = find_best_order(s, r, c, ws, customer);
                if (cost < min_cost) {
                    min_cost = cost;
                    best_ws = ws;
                }
            }
        }
    }

    // Trajects of size 3
    if (best_ws.empty()) {
        for (size_t i = 0; i < s.nwarehouses; ++i) {
            for (size_t j = 0; j < s.nwarehouses; ++j) {
                for (size_t k = 0; k < s.nwarehouses; ++k) {
                    std::vector<size_t> ws = {i, j, k};
                    if (i != j && j != k && has_every_goods(s, customer.orders, ws)) {
                        size_t cost = find_best_order(s, r, c, ws, customer);
                        if (cost < min_cost) {
                            min_cost = cost;
                            best_ws = ws;
                        }
                    }
                }
            }
        }
    }

    // Trajects of size 4
    // if (best_ws.empty()) {
    //     for (size_t i = 0; i < s.nwarehouses; ++i) {
    //         for (size_t j = 0; j < s.nwarehouses; ++j) {
    //             for (size_t k = 0; k < s.nwarehouses; ++k) {
    //                 for (size_t l = 0; l < s.nwarehouses; ++l) {
    //                     std::vector<size_t> ws = {i, j, k, l};
    //                     if (i != j && j != k && l != k && has_every_goods(s, customer.orders, ws)) {
    //                         size_t cost = find_best_order(s, r, c, ws, customer);
    //                         if (cost < min_cost) {
    //                             min_cost = cost;
    //                             best_ws = ws;
    //                         }
    //                     }
    //                 }
    //             }
    //         }
    //     }
    // }

    // 1. What if we didn't find anything
    // 2. What if we can't deliver anything in customer in given time

    if (!best_ws.empty()) {
        // We found a perfect match so go for it
        // Return <distance, vector of the warehouse alone>
        return {min_cost, std::move(best_ws)};
    }
    else {
        // Didn't find an exact match
        return {-1, {}};
    }
}


// <client, ids of warehouses to visit for the products>
std::pair<size_t, std::vector<size_t>> find_best_customer(Simulation& s, Drone d) {
    // Consider every client whose total weight fits in one drone
    // <total weight, customer>
    std::vector<std::pair<size_t, size_t>> potential_clients;
    for (size_t i = 0; i < s.customers.size(); ++i) {
        size_t total_weight = customer_weight(s.products, s.customers[i].orders);
        if (total_weight <= s.max_payload) {
            potential_clients.emplace_back(total_weight, i);
        }
    }

    if (potential_clients.size() > 0) {
        // Minimize traject time
        size_t customer_id;
        size_t min_cost = s.nturns + 1;
        std::vector<size_t> warehouses;
        for (const auto& target: potential_clients) {
            auto path = find_minimum_path(s, d.r, d.c, s.customers[target.second]);
            if (!path.second.empty() && path.first < min_cost) {
                customer_id = target.second;
                min_cost = path.first;
                warehouses = std::move(path.second);
            }
        }

        if (!warehouses.empty())
            return {customer_id, std::move(warehouses)};
    }

    // If we reach this point, we didn't find any way to deliver to client in one shot
    // => Sort them by what would be missing if we delivered with this drone and the closest Warehouse
    // TODO: Optimize this, 'cause that is very costly
    std::sort(
        s.customers.begin(),
        s.customers.end(),
        [&s](const Customer& c1, const Customer& c2) {
            size_t w1 = closest_warehouse(s, c1.id);
            size_t w2 = closest_warehouse(s, c2.id);
            return missing_goods(s, c1.orders, {w1}) < missing_goods(s, c2.orders, {w2});
    });

    for (size_t ci = 0; ci < s.customers.size(); ++ci) {
        const Customer& c = s.customers[ci];
        auto path = find_minimum_path(s, d.r, d.c, c);
        if (!path.second.empty()) {
            return {ci, path.second};
        }
    }

    return {0, {closest(s.warehouses, s.customers[0].r, s.customers[0].c)}};
}


inline std::tuple<Drone, size_t, std::vector<Instr>> get_best_move(Simulation& s, Drone d) {
    // 1. find best customer: <customer_index, list of warehouses to visit>
    std::pair<size_t, std::vector<size_t>> customer = find_best_customer(s, d);
    if (customer.second.empty()) {
        fprintf(stderr, "WRONG\n");
        std::vector<Instr> instr;
        return std::make_tuple(std::move(d), s.nturns, std::move(instr));
    }
    else {
        // 2. deliver product: <new drone, cost, list of instructions>
        return deliver(s, d, s.customers[customer.first], customer.second);
    }
}


void solve(Simulation& s)
{
    // Store resulting instructions
    std::vector<Instr> instructions;

    // Create drone collection
    for (size_t i = 0; i < s.ndrones; ++i) {
        s.drones.push({i, 0, s.warehouses[0].r, s.warehouses[0].c});
    }

    while (!s.drones.empty() && !s.customers.empty()) {
        // display_simulation(s);

        // 1. get next available drone
        Drone drone = s.drones.top();
        s.drones.pop();
        fprintf(stderr, "%zu %zu\n", drone.id, drone.available);

        // 3. Find best move for `drone`
        std::tuple<Drone, size_t, std::vector<Instr>> best_move = get_best_move(s, drone);

        // Update instructions
        instructions.insert(instructions.end(),
                            std::get<2>(best_move).begin(),
                            std::get<2>(best_move).end());

        // Update next availability for this drone
        drone = std::get<0>(best_move);
        drone.available += std::get<1>(best_move) + 1;
        if (drone.available < s.nturns) {
            s.drones.push(drone);
        }
    }

    display_instructions(instructions);
}


//
// READING INPUTS
//

void read_products(Simulation& s)
{
    std::cin >> s.nproducts;
    s.products.resize(s.nproducts, 0);

    for (size_t i = 0; i < s.nproducts; ++i) {
        std::cin >> s.products[i];
    }
}

void read_warehouses(Simulation& s)
{
    std::cin >> s.nwarehouses;
    s.warehouses.reserve(s.nwarehouses);

    size_t r, c;
    for (size_t i = 0; i < s.nwarehouses; ++i) {
        // Read position of warehouse
        std::cin >> r >> c;
        // Read products of warehouse
        Warehouse w{r, c, {}};
        w.products.resize(s.nproducts, 0);
        for (size_t j = 0; j < s.nproducts; ++j) {
            std::cin >> w.products[j];
        }
        s.warehouses.push_back(std::move(w));
    }
}

void read_customers(Simulation& s)
{
    std::cin >> s.nclients;
    size_t r, c, Li, p;
    s.customers.reserve(s.nclients);

    for (size_t i = 0; i < s.nclients; ++i) {
        std::cin >> r >> c >> Li;
        Customer customer{i, r, c, {}};
        for (size_t j = 0; j < Li; ++j) {
            std::cin >> p;
            customer.orders.push_back(p);
        }
        s.customers.push_back(std::move(customer));
    }
}


void read_setup(Simulation& s) {
    std::cin >> s.nrows
             >> s.ncols
             >> s.ndrones
             >> s.nturns
             >> s.max_payload;
}


void print_simulation(const Simulation& s) {
    fprintf(stderr, "nrows %zu\n", s.nrows);
    fprintf(stderr, "ncols %zu\n", s.ncols);
    fprintf(stderr, "ndrones %zu\n", s.ndrones);
    fprintf(stderr, "nturns %zu\n", s.nturns);
    fprintf(stderr, "max_payload %zu\n", s.max_payload);
    fprintf(stderr, "nproducts %zu\n", s.nproducts);
    fprintf(stderr, "nwarehouses %zu\n", s.nwarehouses);
    fprintf(stderr, "nclients %zu\n", s.nclients);
}



int main() {
    Simulation s;

    // Parse input
    read_setup(s);
    read_products(s);
    read_warehouses(s);
    read_customers(s);

    build_index(s);
    build_closest_warehouse(s);
    print_simulation(s);
    solve(s);

    return 0;
}
