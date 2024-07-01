#include "algebra.h"
#ifdef PYENV
#include "pyenv.h"
#endif

#include <unity.h>

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

void setUp() {}

void tearDown() {}

void test_factorial() {
    TEST_ASSERT_EQUAL_UINT64(factorial(0), 1);
    TEST_ASSERT_EQUAL_UINT64(factorial(1), 1);
    TEST_ASSERT_EQUAL_UINT64(factorial(2), 2);
    TEST_ASSERT_EQUAL_UINT64(factorial(3), 6);
    TEST_ASSERT_EQUAL_UINT64(factorial(4), 24);
}

void test_gcd() {
    TEST_ASSERT_EQUAL_UINT64(gcd(0, 5), 5);
    TEST_ASSERT_EQUAL_UINT64(gcd(5, 0), 5);

    TEST_ASSERT_EQUAL_UINT64(gcd(7, 5), 1);
    TEST_ASSERT_EQUAL_UINT64(gcd(9, 6), 3);
    TEST_ASSERT_EQUAL_UINT64(gcd(12, 18), 6);
}

bool check_diophantine_solution(
    const int64_t a, const int64_t b, const int64_t c, const diophantine_solution solution
) {
    return a * solution.x + b * solution.y == c;
}

void test_solve_diophantine() {
    int64_t A[] = {57, 14, 25};
    int64_t B[] = {15, 21, 10};
    int64_t C[] = {3,  7,  5};

    const uint8_t data_len = 3u;
    for (uint8_t i = 0; i < data_len; i++)
        TEST_ASSERT_TRUE(check_diophantine_solution(A[i], B[i], C[i], solve_diophantine(A[i], B[i], C[i])));
}

int main(int argc, char** argv) {
#ifdef PYENV
    initialize_pyenv();
#endif

    UNITY_BEGIN();

    RUN_TEST(test_factorial);
    RUN_TEST(test_gcd);
    RUN_TEST(test_solve_diophantine);

#ifdef PYENV
    finalize_pyenv();
#endif

    return UNITY_END();
}
