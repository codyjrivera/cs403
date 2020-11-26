#include <cilk/cilk.h>

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

void enumsort (int *A, int n) {
	int** comparisons = new int*[n];
    cilk_for (int i = 0; i < n; ++i) {
        comparisons[i] = new int[n];
    }

    cilk_for (int i = 0; i < n; ++i) {
        cilk_for (int j = 0; j < n; ++j) {
            if (A[i] > A[j]) {
                comparisons[i][j] = 1;
            } else if (A[i] == A[j]) {
                comparisons[i][j] = (i > j) ? 1 : 0;
            } else {
                comparisons[i][j] = 0;
            }
        }
    }

    int* indices = new int[n];
    int* temp    = new int[n];
    cilk_for (int i = 0; i < n; ++i) {
        indices[i] = reduce(comparisons[i], n, add, 0);
    }
    cilk_for (int i = 0; i < n; ++i) {
        temp[indices[i]] = A[i];
    }
    cilk_for (int i = 0; i < n; ++i) {
        A[i] = temp[i];
    }

    cilk_for (int i = 0; i < n; ++i) {
        delete[] comparisons[i];
    }
    delete[] comparisons;
    delete[] indices;
    delete[] temp;
}
