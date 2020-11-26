#include <iostream>
#include <cstdlib>
#include <cilk/cilk.h>
using namespace std;

void enumsort (int *A, int n);

void display (int *A, int n) {
        for (int j=0; j<n; j++)
                cout << A[j] << " ";
        cout << endl;
}

int main (int argc, char *argv[ ]) {
        int A[ ] = {2,6,3,5,-3,-4,1,-2};
        enumsort(A,8);
        display(A,8);

        int Z[ ] = {20,40,30,20,10,30};
        enumsort(Z,6);
        display(Z,6);
        return 0;
}
