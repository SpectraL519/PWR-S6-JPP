from .common import DiophantineSolution


def factorial(n: int) -> int:
    return 1 if n <= 1 else n * factorial(n - 1)


def gcd(a: int, b: int) -> int:
    return a if b == 0 else gcd(b, a % b)


def extended_gcd(a, b):
    if b == 0:
        return a, 1, 0

    _gcd, x1, y1 = extended_gcd(b, a % b)

    x = y1
    y = x1 - y1 * (a // b)
    return _gcd, x, y

def solve_diophantine(a: int, b: int, c: int) -> DiophantineSolution:
    _gcd, x, y = extended_gcd(a, b)
    if c % _gcd != 0:
        return DiophantineSolution(0, 0)

    factor = c // _gcd
    return DiophantineSolution(x * factor, y * factor)
