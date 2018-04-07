
#include <iostream>
#include <vector>
#include <algorithm>


void print_array(const std::vector<int>& arr) {
    for (auto v: arr) {
        std::cout << v << ' ';
    }
    std::cout << std::endl;
}

int main() {
    bool debug = false;
    int t, n, v;

    std::cin >> t;
    for (int testcase = 1; testcase <= t; ++testcase) {
        // Read input
        std::cin >> n;
        std::vector<int> odd;
        odd.reserve(n / 2 + 1);
        std::vector<int> eve;
        odd.reserve(n / 2 + 1);

        if (debug) std::cout << "Original ";

        for (int j = 0; j < n; ++j) {
            std::cin >> v;

            if (debug) { std::cout << v << ' '; }

            if (j % 2 == 0) {
                eve.push_back(v);
            } else {
                odd.push_back(v);
            }
        }
        if (debug) { std::cout << std::endl; }

        if (debug) {
            std::cout << "Original" << std::endl;;
            print_array(eve);
            print_array(odd);
        }

        std::sort(eve.begin(), eve.end());
        std::sort(odd.begin(), odd.end());

        if (debug) {
            std::cout << "Sorted" << std::endl;
            print_array(eve);
            print_array(odd);
        }

        std::vector<int> arr;
        arr.reserve(n);
        for (unsigned i = 0; i < odd.size(); ++i) {
            arr.push_back(eve[i]);
            arr.push_back(odd[i]);
        }

        if (eve.size() > odd.size()) {
            arr.push_back(eve[eve.size() - 1]);
        }

        if (debug) {
            std::cout << "Arr" << std::endl;
            print_array(arr);
        }

        // Check if it was sorted properly

        // bool done = false;
        // while (!done) {
        //     done = true;
        //     for (unsigned i = 0; i < arr.size() - 2; ++i) {
        //         if (arr[i] > arr[i + 2]) {
        //             done = false;
        //             int tmp = arr[i];
        //             arr[i] = arr[i + 2];
        //             arr[i + 2] = tmp;
        //         }
        //     }
        // }

        // Check if array was sorted properly
        int not_sorted = -1;
        for (unsigned i = 1; i < arr.size(); ++i) {
            if (arr[i - 1] > arr[i]) {
                not_sorted = i - 1;
                break;
            }
        }

        if (not_sorted == -1) {
            std::cout << "Case #" << testcase << ": OK" << std::endl;
        } else {
            std::cout << "Case #" << testcase << ": " << not_sorted << std::endl;
        }

        // for (auto v: arr) {
        //     std::cout << "ARR " << v << std::endl;
        // }
    }

    return 0;
}
