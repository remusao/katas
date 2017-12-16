#include <iostream>
#include <vector>


int insertion_sort(std::vector<int> arr)
{
    int shifts = 0;
    for (size_t i = 1; i < arr.size(); ++i) {
        size_t j = i;
        while (j > 0 && arr[j - 1] > arr[j]) {
            int tmp = arr[j];
            arr[j] = arr[j - 1];
            arr[j - 1] = tmp;
            --j;
            ++shifts;
        }
    }

    return shifts;
}


int partition(std::vector<int>& vec, int left, int right)
{
    int pivot = vec[right];
    int i = left;
    int tmp;

    for (int j = left; j < right; ++j) {
        if (vec[j] <= pivot) {
            tmp = vec[j];
            vec[j] = vec[i];
            vec[i] = tmp;
            ++i;
        }
    }

    tmp = vec[i];
    vec[i] = vec[right];
    vec[right] = tmp;

    return i;
}


int quicksort(std::vector<int>& vec, int left, int right)
{
    int swaps = 0;
    if (left < right)
    {
        int pivot_index = partition(vec, left, right);
        swaps += pivot_index + 1 - left;
        swaps += quicksort(vec, left, pivot_index - 1);
        swaps += quicksort(vec, pivot_index + 1, right);
    }

    return swaps;
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

    int insertion_shifts = insertion_sort(arr);
    int quicksort_swaps = quicksort(arr, 0, arr.size() - 1);
    std::cout << insertion_shifts - quicksort_swaps << std::endl;

    return 0;
}
