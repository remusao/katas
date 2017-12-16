#include <iostream>
#include <vector>


void display_array(const std::vector<int>& arr)
{
    if (arr.size() > 0) {
        auto it = arr.cbegin();
        std::cout << *it;
        ++it;

        while (it != arr.cend()) {
            std::cout << ' ' << *it;
            ++it;
        }

        std::cout << std::endl;
    }
}


void insertion_sort(std::vector<int>& arr)
{
    for (int i = 1; i < arr.size(); ++i) {
        int j = i;
        while (j > 0 && arr[j - 1] > arr[j]) {
            int tmp = arr[j];
            arr[j] = arr[j - 1];
            arr[j - 1] = tmp;
            --j;
        }
        display_array(arr);
    }
}


int main(void)
{
    int n, elt;
    std::vector<int> arr;
    std::cin >> n;
    arr.reserve(n);

    while (n--) {
        std::cin >> elt;
        arr.push_back(elt);
    }

    insertion_sort(arr);

    return 0;
}
