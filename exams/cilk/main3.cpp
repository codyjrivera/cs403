#include <iostream>
#include <cstdlib>
#include <cilk/cilk.h>
using namespace std;

int f(int x, int y) { return x+y; }
int g(int x) { return x*x; }
int p(int x) { return x>10; }

int fun (int (*f)(int,int), int (*g)(int), int (*p)(int), int id, int *A, int low, int high);

int main (int argc, char *argv[ ]) {
        int A[ ] = {2,6,3,5,-3,-4,1,-2};
        int result = fun(f,g,p,0,A,0,7);
	cout << "Result = " << result << endl;
        return 0;
}
