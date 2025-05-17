#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int i = 0;
int n = 0;
int arr[10];

int bubblesort() {
    int i = 0;
    int j;
    int tmp;
    for (i = 0; i <= n - 2; i++) {
        for (j = 0; j <= (n - 2 - i); j++) {
            if ((arr[j] > arr[j + 1])) {
                tmp = arr[j + 1];
                arr[j + 1] = arr[j];
                arr[j] = tmp;
            }
        }
        // for_end_93998877091184
    }
    // for_end_93998877091312
    return 0;
}

int main() {
    n = 10;
    arr[0] = 4;
    arr[1] = 3;
    arr[2] = 9;
    arr[3] = 2;
    arr[4] = 0;
    arr[5] = 1;
    arr[6] = 6;
    arr[7] = 5;
    arr[8] = 7;
    arr[9] = 8;
    for (i = bubblesort(); i <= n - 1; i++) {
        printf("%d", arr[i]);
    }
    // for_end_93998877106864
    return 0;
}
