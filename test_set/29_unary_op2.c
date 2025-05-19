#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int a = 0;
int b = 0;

int main() {
    a = 56;
    b = 4;
    a = a - -(4) + b;
    if (((-(~(~(~(a)))) != 65))) {
        a = -(-(-(1)));
    } else {
        a = 0 + b;
    }
    printf("%d", a);
    return 0;
}
