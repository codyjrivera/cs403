#include <cilk/cilk.h>

int add(int a, int b) {
    return a + b;
}

template <typename T>
void prefixSum(T* array, int n, T (*op)(T, T)) {
    if (n <= 1) return;
    int  sn     = n / 2;
    int* sarray = new int[sn];
    cilk_for (int i = 0; i < sn; ++i) {
        sarray[i] = op(array[(2 * i)], array[(2 * i) + 1]);
    }
    prefixSum(sarray, sn, op);
    cilk_for (int i = 0; i < sn; ++i) {
        array[(2 * i) + 1] = sarray[i];
    }
    cilk_for (int i = 0; i < (n + 1) / 2; ++i) {
        array[(2 * i)] = op(array[(2 * i)], sarray[i - 1]);
    }
    delete[] sarray;
}

int fun (int (*f)(int,int), int (*g)(int), int (*p)(int), int id, int *A, int low, int high) {
    int n = high - low + 1;
    if (n <= 0) return id;

	int* temp1 = new int[n];
    cilk_for (int i = 0; i < n; ++i) {
        temp1[i] = g(A[low + i]);
    }

    int* sat = new int[n];
    cilk_for (int i = 0; i < n; ++i) {
        sat[i] = p(temp1[i]);
    }
    prefixSum(sat, n, add);

    int m = sat[n - 1];
    if (m <= 0) return id;
    int* temp2 = new int[m];
    cilk_for (int i = 0; i < n; ++i) {
        if (p(temp1[i])) {
            temp2[sat[i] - 1] = temp1[i];
        }
    }
    prefixSum(temp2, m, f);
    int result = temp2[m - 1];
    
    delete[] temp1;
    delete[] sat;
    delete[] temp2;
    return result;
}
