#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int a = 0;
int b = 0;

int main() {
    a = 10;
    b = -(1);
    printf("%d", a + b);
    return 0;
}
