#include <iostream>
#include <vector>


void insertion_sort(std::vector<int>& arr)
{
    int shifts = 0;
    for (int i = 1; i < arr.size(); ++i) {
        int j = i;
        while (j > 0 && arr[j - 1] > arr[j]) {
            int tmp = arr[j];
            arr[j] = arr[j - 1];
            arr[j - 1] = tmp;
            --j;
            ++shifts;
        }
    }
    std::cout << shifts << std::endl;
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
