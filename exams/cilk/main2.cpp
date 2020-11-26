#include <iostream>
#include <cstdlib>
#include <cilk/cilk.h>
using namespace std;

int *multpoly (int *p, int m, int *q, int n);

void display (int *A, int n) {
        for (int j=0; j<=n; j++)
                cout << A[j] << " ";
        cout << endl;
}

int main (int argc, char *argv[ ]) {
        int m = atoi(argv[1]);
        int n = atoi(argv[2]);

        int *A = new int[m+1];
        for (int j=0; j<=m; j++)
                A[j] = 2*j+1;
	display (A, m);

        int *B = new int[n+1];
        for (int j=0; j<=n; j++)
                B[j] = 2*(j+1);
	display (B, n);

        int *R = multpoly (A, m, B, n);
	display (R, m+n);
        return 0;
}
