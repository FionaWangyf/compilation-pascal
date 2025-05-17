#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
int a;
int main()
{
a = 3;
printf("%d", a);
return 0;
}
