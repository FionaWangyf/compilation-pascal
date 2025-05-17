#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int a = 0;

int main() {
    a = 60;
    printf("%d", ~(a));
    return 0;
}
