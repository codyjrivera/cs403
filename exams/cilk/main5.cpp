#include <iostream>
#include <cstdlib>
#include <cilk/cilk.h>
using namespace std;

void Floyd (int ***d, int n);

void display (int ***X,  int n) {
        for (int i=1; i<=n; i++) {
                for (int j=1; j<=n; j++)
                        cout << X[n][i][j] << "\t";
                cout << endl;
        }
}

const int n=4;

int main (int argc, char *argv[ ]) {
	int a[4][4] = {{0,99,-2,5},{4,0,3,99},{99,99,0,2},{99,-1,99,0}};

        int ***d = new int**[n+1];
        for (int k=0; k<=n; k++) {
                d[k] = new int*[n+1];
                for (int i=1; i<=n; i++)
                	d[k][i] = new int[n+1];
        }

        for (int i=1; i<=n; i++) {
                for (int j=1; j<=n; j++)
                        d[0][i][j] = a[i-1][j-1];
        }

	Floyd (d, 4);
        display (d, 4);
        return 0;
}
