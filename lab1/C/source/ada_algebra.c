#include "algebra.h"

uint64_t factorial(const uint64_t n) {
#if ADA_RECURSIVE == 1
    return Ada_R_Factorial(n);
#else
    return Ada_Factorial(n);
#endif
}

uint64_t gcd(uint64_t a, uint64_t b) {
#if ADA_RECURSIVE == 1
    return Ada_R_GCD(a, b);
#else
    return Ada_GCD(a, b);
#endif
}

extern diophantine_solution Ada_Solve_Diophantine(int64_t a, int64_t b, int64_t c);
extern diophantine_solution Ada_R_Solve_Diophantine(int64_t a, int64_t b, int64_t c);

diophantine_solution solve_diophantine(int64_t a, int64_t b, int64_t c) {
    diophantine_solution solution;
#if ADA_RECURSIVE == 1
    solution = Ada_R_Solve_Diophantine(a, b, c);
#else
    solution = Ada_Solve_Diophantine(a, b, c);
#endif
    return solution;
}
