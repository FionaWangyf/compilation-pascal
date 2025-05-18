#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

const double pi = 3.14159265359;
const double two_pi = 6.28318530718;
const double epsilon = 1e-06;
float test_block[8][8];
float test_dct[8][8];
float test_idct[8][8];
int dim_x = 0;
int dim_y = 0;
int i = 0;
int j = 0;

float my_fabs(float x) {
    float res = 0;
    if ((x > 0)) {
        res = x;
    } else {
        res = -(x);
    }
    return res;
}

float p(float x) {
    float res = 0;
    res = 3 * x - 4 * x * x * x;
    return res;
}

float my_sin_impl(float x) {
    float res = 0;
    if ((my_fabs(x) <= epsilon)) {
        res = x;
    } else {
        res = p(my_sin_impl(x / 3));
    }
    return res;
}

float my_sin(float x) {
    int xx;
    float res = 0;
    if (((x > two_pi)) || ((x < -(two_pi)))) {
        xx = 1;
        x = x - 1;
    }
    if ((x > pi)) {
        x = x - two_pi;
    }
    if ((x < -(pi))) {
        x = x + two_pi;
    }
    res = my_sin_impl(x);
    return res;
}

float my_cos(float x) {
    float res = 0;
    res = my_sin(x * pi / 2);
    return res;
}

void write_mat(int n, int m) {
    int i = 0;
    int j = 0;
    for (i = 0; i <= n - 1; i++) {
        printf("%.6f", test_dct[i][0]);
        for (j = 1; j <= m - 1; j++) {
            printf("%.6f", test_dct[i][j]);
        }
    }
}

void write_mat2(int n, int m) {
    int i = 0;
    int j = 0;
    for (i = 0; i <= n - 1; i++) {
        printf("%.6f", test_idct[i][0]);
        for (j = 1; j <= m - 1; j++) {
            printf("%.6f", test_idct[i][j]);
        }
    }
}

void dct(int n, int m) {
    int u;
    int v;
    int i = 0;
    int j = 0;
    for (u = 0; u <= n - 1; u++) {
        for (v = 0; v <= m - 1; v++) {
            test_dct[u][v] = 0;
            for (i = 0; i <= n - 1; i++) {
                for (j = 0; j <= m - 1; j++) {
                    test_dct[u][v] = test_dct[u][v] + test_block[i][j] * my_cos(pi / n * (i + 1 / 2) * u) * my_cos(pi / m * (i + 1 / 2) * v);
                }
            }
        }
    }
}

void idct(int n, int m) {
    int u;
    int v;
    int i = 0;
    int j = 0;
    for (u = 0; u <= n - 1; u++) {
        for (v = 0; v <= m - 1; v++) {
            test_idct[u][v] = 1 / 4 * test_dct[0][0];
            for (i = 1; i <= n - 1; i++) {
                test_idct[u][v] = test_idct[u][v] + 1 / 2 * test_dct[i][0];
            }
            for (j = 1; j <= m - 1; j++) {
                test_idct[u][v] = test_idct[u][v] + 1 / 2 * test_dct[0][j];
            }
            for (i = 1; i <= n - 1; i++) {
                for (j = 1; j <= m - 1; j++) {
                    test_idct[u][v] = test_idct[u][v] + test_dct[i][j] * my_cos(pi / n * (u + 1 / 2) * i) * my_cos(pi / m * (v + 1 / 2) * j);
                }
            }
            test_idct[u][v] = test_idct[u][v] * 2 / n * 2 / m;
        }
    }
}

int main() {
    dim_x = 0;
    dim_y = 0;
    scanf("%d", &dim_x);
    scanf("%d", &dim_y);
    for (i = 0; i <= dim_x - 1; i++) {
        for (j = 0; j <= dim_y - 1; j++) {
            scanf("%f", &test_block[i][j]);
        }
    }
dct(dim_x, dim_y);
write_mat(dim_x, dim_y);
idct(dim_x, dim_y);
write_mat2(dim_x, dim_y);
    return 0;
}
