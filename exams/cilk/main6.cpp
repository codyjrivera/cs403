#include <iostream>
#include <cstdlib>
#include <cilk/cilk.h>
using namespace std;

void APSP (int ***d, int n);

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
        for (int m=1; m<=n; m++) {
                d[m] = new int*[n+1];
                for (int i=1; i<=n; i++)
                	d[m][i] = new int[n+1];
        }

        for (int i=1; i<=n; i++) {
                for (int j=1; j<=n; j++)
                        d[1][i][j] = a[i-1][j-1];
        }

	APSP (d, 4);
        display (d, 4);
        return 0;
}
