#include <algorithm> /* std::min, std::max */
#include <climits>   /* INT_MAX, identity element */
#include <cilk/cilk.h>

int min(int a, int b) {
    return std::min(a, b);
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

void APSP (int ***D, int n) {
    int lastM;
    // Powers of 2
	for (int m = 2; m <= n; m *= 2) {
        cilk_for (int i = 1; i <= n; ++i) {
            cilk_for (int j = 1; j <= n; ++j) {
                int* sums = new int[n];
                cilk_for (int k = 0; k < n; ++k) {
                    sums[k] = D[m / 2][i][k + 1] + D[m / 2][k + 1][j];
                }
                int mv = reduce(sums, n, min, INT_MAX);
                D[m][i][j] = std::min(mv, D[m / 2][i][j]);
                delete[] sums;
            }
        }
        lastM = m;
    }

    // Odd ones out
	for (int m = lastM + 1; m <= n; ++m) {
        cilk_for (int i = 1; i <= n; ++i) {
            cilk_for (int j = 1; j <= n; ++j) {
                int* sums = new int[n];
                cilk_for (int k = 0; k < n; ++k) {
                    sums[k] = D[m - 1][i][k + 1] + D[1][k + 1][j];
                }
                int mv = reduce(sums, n, min, INT_MAX);
                D[m][i][j] = std::min(mv, D[m - 1][i][j]);
                delete[] sums;
            }
        }
    }
}
