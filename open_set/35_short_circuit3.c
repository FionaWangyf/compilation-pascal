#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

const char aa = 'A';
const char bb = 'B';
const char cc = 'C';
const char dd = 'D';
const char e = 'E';
const char f = 'F';
const char g = 'G';
const char h = 'H';
const char i = 'I';
const char j = 'J';
const char k = 'K';
const int c = 1;
int a = 0;
int b = 0;
int d = 0;
int i0 = 0;
int i1 = 0;
int i2 = 0;
int i3 = 0;
int i4 = 0;

int set_a(int val) {
    a = val;
    return val;
}

int set_b(int val) {
    b = val;
    return val;
}

int set_d(int val) {
    d = val;
    return val;
}

int main() {
    a = 2;
    b = 3;
    if (((set_a(0) != 0)) && ((set_b(1) != 0))) {
    }
    printf("%d", a);
    printf("%d", b);
    a = 2;
    b = 3;
    if (((set_a(0) != 0)) && ((set_b(1) != 0))) {
    }
    printf("%d", a);
    printf("%d", b);
    d = 2;
    if (((c >= 1)) && ((set_d(3) != 0))) {
    }
    printf("%d", d);
    if (((c <= 1)) || ((set_d(4) != 0))) {
    }
    printf("%d", d);
    if (((16 >= (3 - (2 + 1))))) {
        printf("%d", aa);
    }
    if (((25 - 7) != (36 - 6 * 3))) {
        printf("%d", bb);
    }
    if (((1 != (7 % 2)))) {
        printf("%d", cc);
    }
    if ((3 <= 4)) {
        printf("%d", dd);
    }
    if ((0 != 0)) {
        printf("%d", e);
    }
    if ((1 != 0)) {
        printf("%d", f);
    }
    i0 = 0;
    i1 = 1;
    i2 = 2;
    i3 = 3;
    i4 = 4;
    if (((i0 != 0)) || ((i1 != 0))) {
        printf("%d", g);
    }
    if (((i0 >= i1)) || ((i1 <= i0))) {
        printf("%d", h);
    }
    if (((i2 >= i1)) && ((i4 != i3))) {
        printf("%d", i);
    }
    if (((i0 == 0)) && ((i3 < i3)) || ((i4 >= i4))) {
        printf("%d", j);
    }
    if (((i0 == 0)) || ((i3 < i3)) && ((i4 >= i4))) {
        printf("%d", k);
    }
    return 0;
}
