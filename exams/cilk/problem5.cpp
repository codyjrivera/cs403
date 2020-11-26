#include <algorithm>  /* std::min, std::max */
#include <cilk/cilk.h>

void Floyd (int ***d, int n) {
	for (int k = 1; k <= n; ++k) {
        cilk_for (int i = 1; i <= n; ++i) {
            cilk_for (int j = 1; j <= n; ++j) {
                int cand = d[k - 1][i][k] + d[k - 1][k][j];
                d[k][i][j] = std::min(cand, d[k - 1][i][j]);
            }
        }
    }
}
