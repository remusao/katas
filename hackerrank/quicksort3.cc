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


void quicksort(std::vector<int>& vec, int left, int right)
{
    if (left < right)
    {
        int pivot_index = partition(vec, left, right);
        print_vec(vec, 0, vec.size() - 1);
        quicksort(vec, left, pivot_index - 1);
        quicksort(vec, pivot_index + 1, right);
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
