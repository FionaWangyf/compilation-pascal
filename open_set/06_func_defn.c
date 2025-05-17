#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int a;
int b;

int func(int p) {
    p = p - 1;
    return p;
}

int main() {
    a = 10;
    b = func(a);
    printf("%d", b);
    return 0;
}
