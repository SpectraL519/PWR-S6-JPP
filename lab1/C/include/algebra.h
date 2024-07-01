#ifndef ALGEBRA_H
#define ALGEBRA_H

#include <stdint.h>

typedef struct diophantine_solution {
    int64_t x;
    int64_t y;
} diophantine_solution;

uint64_t factorial(const uint64_t);
uint64_t gcd(uint64_t, uint64_t);
diophantine_solution solve_diophantine(int64_t, int64_t, int64_t);

#endif // ALGEBRA_H
