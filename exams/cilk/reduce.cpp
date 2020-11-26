#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk.h>

template <typename T>
T reduce(T* array, int n, T (*op)(T, T), T id) {
    if (n < 1) return id;
    else if (n == 1) return array[0];
    T* work1 = new T[n];
    T* work2 = new T[n];
    cilk_for (int i = 0; i < n; ++i) {
        work1[i] = array[i];
    }
    for (int s = n; s > 1; s /= 2) {
        if (s % 2 == 1) {
            work1[s - 2] = op(work1[s - 2], work1[s - 1]);
        }
        cilk_for (int i = 0; i < s / 2; ++i) {
            work2[i] = op(work1[(2 * i)], work1[(2 * i) + 1]);
        }
        T* temp;
        temp = work1; work1 = work2; work2 = temp;
    }
    T val = work1[0];
    delete[] work1;
    delete[] work2;
    return val;
}

int add(int a, int b) {
    return a + b;
}

int main(int argc, char** argv) {
    int s = argc - 1;
    int* array = new int[s];
    cilk_for (int i = 0; i < s; ++i) {
        array[i] = atoi(argv[i + 1]);
    }
    printf("Reduction result: %d\n", reduce(array, s, add, 0));
    return 1;
}