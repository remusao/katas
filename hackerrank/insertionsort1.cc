#include <iostream>
#include <vector>

void display_array(const std::vector<int>& arr) {
    auto it = arr.cbegin();
    if (it != arr.cend()) {
        std::cout << *it;
        ++it;
        while (it != arr.cend()) {
            std::cout << ' ' << *it;
            ++it;
        }
        std::cout << std::endl;
    }
}


void insertion_sort(std::vector<int>&  arr)
{
    int index = arr.size() - 1;
    while (index > 0 && arr[index - 1] > arr[index]) {
        int tmp = arr[index];
        arr[index] = arr[index - 1];
        display_array(arr);
        arr[index - 1] = tmp;
        --index;
    }

    if (index != arr.size() - 1) {
        display_array(arr);
    }
}


int main(void)
{
    std::vector<int>  arr;
    int n, elt;
    std::cin >> n;
    arr.reserve(n);

    while (n--) {
        std::cin >> elt;
        arr.push_back(elt);
    }

    insertion_sort(arr);

    return 0;
}
