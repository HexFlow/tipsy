#include <stdio.h>

int main() {
    float y;
    int x;
    scanf("%d%f", &x, &y);
    if (x + 0.5 > y)
        printf("\n%.2f", y);
    else if (x % 5 == 0)
        printf("\n%.2f", y);
    else {
        printf("\n%.2f", y - x);
    }
    return 0;
}
