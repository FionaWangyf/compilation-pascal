#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int a;

int defn() {
    return 4;
}

int main() {
    a = defn();
    printf("%d", a);
    return 0;
}
