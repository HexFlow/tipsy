#include <stdio.h>

int main() {
    float y;
    int x;
    scanf("%d%f", &x, &y);
    if (x + .5 > y)
        printf("\n%.2f", y);
    else if (x % 5 != 0)
        printf("\n%.2f", y);
    else {
        printf("\n%.2f", y - x - .5);
    }
    return 0;
}
