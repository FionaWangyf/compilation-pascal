#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int n = 0;
int i = 0;
int arr[10];

void selectsort() {
    int i = 0;
    int j;
    int min;
    int tmp;
    for (i = 0; i <= n - 2; i++) {
        min = i;
        for (j = i + 1; j <= n - 1; j++) {
            if ((arr[min] > arr[j])) {
                min = j;
            }
        }
        // for_end_94758578992544
        if ((min != i)) {
            tmp = arr[min];
            arr[min] = arr[i];
            arr[i] = tmp;
        }
    }
    // for_end_94758578997872
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
    selectsort();
    for (i = 0; i <= n - 1; i++) {
        printf("%d", arr[i]);
    }
    // for_end_94758579012800
    return 0;
}
