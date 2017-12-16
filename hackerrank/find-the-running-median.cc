#include <vector>
#include <iostream>
#include <functional>
#include <cstdio>


template <typename T>
class Heap {
    public:
        Heap(int min=false) : min_(min) {}

        inline const T& top() const { return heap_[0]; }
        inline size_t size() const { return heap_.size(); }

        T pop() {
            T top = heap_[0];
            heap_[0] = heap_.back();
            heap_.pop_back();
            heapify_(0);

            return top;
        }

        void insert(const T& elt) {
            heap_.push_back(elt);
            size_t index = heap_.size() - 1;
            size_t parent = parent_(index);
            while (parent >= 0 && cmp_(heap_[index], heap_[parent])) {
                // Swap parent with child
                T tmp = heap_[index];
                heap_[index] = heap_[parent];
                heap_[parent] = tmp;
                // Go up
                index = parent;
                parent = parent_(index);
            }
        }

    private:
        // Attributes
        bool min_;
        std::vector<T> heap_;

        // Methods
        size_t parent_(int i) { return (i - 1) / 2; }
        size_t left_(int i) { return (i + 1) * 2 - 1; }
        size_t right_(int i) { return (i + 1) * 2; }

        inline bool cmp_(const T& a, const T& b) {
            if (min_) return a < b;
            else return a > b;
        }

        void heapify_(int start) {
            // Find children with largest value
            size_t largest = left_(start);
            if (largest >= heap_.size()) return;

            size_t right_child = right_(start);
            if (right_child < heap_.size() && cmp_(heap_[right_child], heap_[largest])) {
                largest = right_child;
            }

            // Heapify subtree
            if (cmp_(heap_[largest], heap_[start])) {
                T tmp = heap_[start];
                heap_[start] = heap_[largest];
                heap_[largest] = tmp;
                heapify_(largest);
            }
        }
};


int main() {
    Heap<int> max_heap;
    Heap<int> min_heap(true);

    int n, number;
    std::cin >> n;

    for (int i = 0; i < n; ++i) {
        std::cin >> number;

        if (min_heap.size() > 0 && number >= min_heap.top()) {
            min_heap.insert(number);
        }
        else {
            max_heap.insert(number);
        }

        // Balance heaps
        int size = min_heap.size() + max_heap.size();
        if (size % 2 == 0) {
            if (min_heap.size() > max_heap.size()) {
                while (max_heap.size() != min_heap.size()) {
                    max_heap.insert(min_heap.pop());
                }
            }
            else if (min_heap.size() < max_heap.size()) {
                while (max_heap.size() != min_heap.size()) {
                    min_heap.insert(max_heap.pop());
                }
            }
        }
        else {
            if (min_heap.size() > max_heap.size()) {
                while (max_heap.size() < min_heap.size()) {
                    max_heap.insert(min_heap.pop());
                }
            }
            else if ((min_heap.size() + 1) < max_heap.size()) {
                while (max_heap.size() != (min_heap.size() + 1)) {
                    min_heap.insert(max_heap.pop());
                }
            }
        }

        // Get median element
        float median = max_heap.top();
        if ((i + 1) % 2 == 0) {
            median += min_heap.top();
            median /= 2.0;
        }
        printf("%.1f\n", median);
    }
}
