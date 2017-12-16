#include <iostream>
#include <vector>


void print_vec(const std::vector<int>& vec, int left, int right)
{
    while (left <= right)
    {
        std::cout << vec[left] << ' ';
        ++left;
    }

    std::cout << std::endl;
}

int select_pivot(const std::vector<int>& vec, int left, int right)
{
    return vec[left];
}


std::pair<int, int> partition(std::vector<int>& vec, int left, int right)
{
    std::vector<int> buffer;
    buffer.reserve(right - left + 1);
    int pivot = vec[left];

    // Less than pivot
    for (int i = left; i <= right; ++i) {
        if (vec[i] < pivot) {
            buffer.push_back(vec[i]);
        }
    }

    int pivot_left, pivot_right;
    pivot_left = buffer.size();

    // Equal too pivot
    for (int i = left; i <= right; ++i) {
        if (vec[i] == pivot) {
            buffer.push_back(vec[i]);
        }
    }
    pivot_right = buffer.size() - 1;

    // Greater than pivot
    for (int i = left; i <= right; ++i) {
        if (vec[i] > pivot) {
            buffer.push_back(vec[i]);
        }
    }

    // Copy in original array
    for (int i = left; i <= right; ++i) {
        vec[i] = buffer[i - left];
    }

    return {pivot_left, pivot_right};
}


void quicksort(std::vector<int>& vec, int left, int right)
{
    if (left < right)
    {
        std::pair<int, int> pivot_index = partition(vec, left, right);
        quicksort(vec, left, left + pivot_index.first - 1);
        quicksort(vec, left + pivot_index.second + 1, right);
        print_vec(vec, left, right);
    }
}


int main()
{
    int n, elt;
    std::cin >> n;

    std::vector<int> vec;
    vec.reserve(n);

    while (n--) {
        std::cin >> elt;
        vec.push_back(elt);
    }

    quicksort(vec, 0, vec.size() - 1);
}
