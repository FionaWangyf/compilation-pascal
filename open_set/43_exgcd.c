#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int x[1];
int y[1];
int a = 0;
int b = 0;

int exgcd(int a, int b, int *x, int *y) {
    int t;
    int r;
    if ((b == 0)) {
        x = 1;
        y = 0;
        return a;
    } else {
        r = exgcd(b, a % b, x, y);
        t = x;
        x = y;
        y = (t - (a / b) * y);
        return r;
    }
}

int main() {
    a = 7;
    b = 15;
    x[0] = 1;
    y[0] = 1;
    exgcd(a, b, x[0], y[0]);
    x[0] = ((x[0] % b) + b) % b;
    printf("%d", x[0]);
    return 0;
}
