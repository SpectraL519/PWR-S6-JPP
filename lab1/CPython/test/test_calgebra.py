import pytest
from pygebra import calgebra


@pytest.mark.parametrize(
    "n, expected",
    [(0, 1), (1, 1), (2, 2), (3, 6), (4, 24)]
)
def test_factorial(n, expected):
    assert calgebra.factorial(n) == expected


@pytest.mark.parametrize(
    "a, b, expected",
    [(0, 5, 5), (5, 0, 5), (7, 5, 1), (9, 6, 3), (12, 18, 6)]
)
def test_gcd(a, b, expected):
    assert calgebra.gcd(a, b) == expected


@pytest.mark.parametrize(
    "a, b, c",
    [(57, 15, 3), (14, 21, 7), (25, 10, 5)]
)
def test_solve_diophantine(a, b, c):
    assert calgebra.solve_diophantine(a, b, c).check(a, b, c)


if __name__ == "__main__":
    pytest.main()
