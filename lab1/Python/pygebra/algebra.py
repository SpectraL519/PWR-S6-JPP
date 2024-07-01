from .common import DiophantineSolution


def factorial(n: int) -> int:
    fact = 1
    if n <= 1:
        return fact

    for i in range(2, n + 1):
        fact *= i
    return fact


def gcd(a: int, b: int) -> int:
    while b > 0:
        a, b = b, a % b
    return a


def extended_gcd(a: int, b: int) -> tuple[int, int, int]:
    x1, y1 = 0, 1
    x2, y2 = 1, 0

    while b != 0:
        quotient = a // b
        remainder = a % b

        x_tmp = x2 - quotient * x1
        y_tmp = y2 - quotient * y1
        a, b = b, remainder
        x2, y2 = x1, y1
        x1, y1 = x_tmp, y_tmp

    x, y = x2, y2
    return a, x, y


def solve_diophantine(a: int, b: int, c: int) -> DiophantineSolution:
    _gcd, x, y = extended_gcd(a, b)
    if c % _gcd != 0:
        return DiophantineSolution(x, y)

    factor = c // _gcd
    return DiophantineSolution(x * factor, y * factor)
