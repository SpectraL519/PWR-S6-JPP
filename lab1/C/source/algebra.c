#include "algebra.h"

uint64_t factorial(const uint64_t n) {
    uint64_t factorial = 1u;

    for (uint64_t i = 2u; i <= n; i++)
        factorial *= i;

    return factorial;
}

uint64_t gcd(uint64_t a, uint64_t b) {
    while (b > 0u) {
        uint64_t tmp = b;
        b = a % b;
        a = tmp;
    }

    return a;
}

int64_t extended_gcd(int64_t a, int64_t b, int64_t* x, int64_t* y) {
    int64_t x1 = 0, y1 = 1;
    int64_t x2 = 1, y2 = 0;

    while (b != 0) {
        int64_t quotient = a / b;
        int64_t remainder = a % b;
        int64_t x_tmp, y_tmp;

        x_tmp = x2 - quotient * x1;
        y_tmp = y2 - quotient * y1;
        a = b;
        b = remainder;
        x2 = x1;
        y2 = y1;
        x1 = x_tmp;
        y1 = y_tmp;
    }

    *x = x2;
    *y = y2;
    return a;
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
