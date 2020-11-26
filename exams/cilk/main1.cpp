#include <iostream>
#include <cstdlib>
#include <cilk/cilk.h>
using namespace std;

int count (int **A, int n, int k);

void display (int **A, int n) {
        for (int j=0; j<n; j++) {
                for (int k=0; k<n; k++)
                        cout << A[j][k] << "\t";
                cout << endl;
        }
        cout << endl;
}

int main (int argc, char *argv[ ]) {
        int n = atoi(argv[1]);
        int k = atoi(argv[2]);

        int **array = new int*[n];
        for (int j=0; j<n; j++) {
                array[j] = new int[n];
                for (int k=0; k<n; k++)
                        array[j][k] = (j+2)*(k+2);
        }
        display (array, n);

        int result = count (array, n, k);
        cout << "Result = " << result << endl;
        return 0;
}
