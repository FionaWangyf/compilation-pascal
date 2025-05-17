#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

const int a = 10;

int main() {
    printf("%d", a + 5);
    return 0;
}
