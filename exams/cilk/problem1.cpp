#include <cilk/cilk.h>
#include <cstdio>

int add(int a, int b) {
    return a + b;
}

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

int count (int **A, int n, int k) {
    if (n == 0) return 0;
    int* r1 = new int[n];
    cilk_for (int i = 0; i < n; ++i) {
        int* r2 = new int[n];
        cilk_for (int j = 0; j < n; ++j) {
            r2[j] = (A[i][j] == k) ? 1 : 0;
        }
        r1[i] = reduce(r2, n, add, 0);
        delete[] r2;
    }
    int total = reduce(r1, n, add, 0);
    delete[] r1;
    return total;
}
