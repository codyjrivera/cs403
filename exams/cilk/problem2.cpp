#include <algorithm> /* use for std::min */
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

int *multpoly (int *p, int m, int *q, int n) {
    // assert m < n, otherwise rename
	if (m < n) {
        return multpoly(q, n, p, m);
    }
    int** products = new int*[m + 1];
    cilk_for (int i = 0; i <= m; ++i) {
        products[i] = new int[n + 1];
    }

    cilk_for (int i = 0; i <= m; ++i) {
        cilk_for (int j = 0; j <= n; ++j) {
            products[i][j] = p[i] * q[j];
        }
    }

    int* newpoly = new int[m + n + 1];
    cilk_for (int i = 0; i <= m + n; ++i) {
        int off = std::max(0, i - m);
        int s   = std::min(i, n - off);
        int* order = new int[s + 1];
        cilk_for (int j = 0; j <= s; ++j) {
            order[j] = products[i - off - j][off + j];
        }
        newpoly[i] = reduce(order, s + 1, add, 0);
        delete[] order;
    }

    cilk_for (int i = 0; i <= m; ++i) {
        delete[] products[i];
    }
    delete[] products;
    return newpoly;
}
