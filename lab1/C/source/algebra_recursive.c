#include "algebra.h"

uint64_t factorial(const uint64_t n) {
    if (n <= 1u)
        return 1u;

    return n * factorial(n - 1);
}

uint64_t gcd(uint64_t a, uint64_t b) {
    if (b == 0u)
        return a;

    return gcd(b, a % b);
}

int64_t extended_gcd(int64_t a, int64_t b, int64_t* x, int64_t* y) {
    if (b == 0) {
        *x = 1;
        *y = 0;
        return a;
    }

    int64_t x1, y1;
    int64_t gcd = extended_gcd(b, a % b, &x1, &y1);

    *x = y1;
    *y = x1 - (a / b) * y1;
    return gcd;
}

diophantine_solution solve_diophantine(int64_t a, int64_t b, int64_t c) {
    int64_t x, y;
    int64_t gcd = extended_gcd(a, b, &x, &y);

    if (c % gcd != 0)
        return (diophantine_solution){ .x = 0, .y = 0 };

    int64_t factor = c / gcd;

    return (diophantine_solution){
        .x = x * factor,
        .y = y * factor
    };
}
