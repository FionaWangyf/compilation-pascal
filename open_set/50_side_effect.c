#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

const char split = ',';
int a = 0;
int b = 0;
int k = 0;
int i = 0;

int inc_a() {
    int temp_b;
    temp_b = a;
    temp_b = temp_b + 1;
    a = temp_b;
    return a;
}

int main() {
    a = -(1);
    b = 1;
    k = 5;
    for (i = 0; i <= k; i++) {
        if (((inc_a() != 0)) && ((inc_a() != 0)) && ((inc_a() != 0))) {
            printf("%d%d%c", a, b, split);
        }
        if (((inc_a() < 14)) || (((inc_a() != 0)) && (((inc_a() - inc_a() + 1) != 0)))) {
            printf("%d%c", a, split);
            b = b * 2;
        } else {
            inc_a();
        }
    }
    printf("%d%d%c", a, b, split);
    printf("%d", a);
    return 0;
}
