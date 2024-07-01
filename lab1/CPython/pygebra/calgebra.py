import os
import ctypes
from .common import DiophantineSolution


def _load_c_algebra_lib():
    current_dir = os.path.dirname(os.path.abspath(__file__))
    so_file_path = os.path.join(current_dir, "algebra.so")
    c_algebra_lib = ctypes.CDLL(so_file_path)
    return c_algebra_lib


_factorial = None
_gcd = None
_solve_diophantine = None


def factorial(n: int) -> int:
    global _factorial
    if _factorial is None:
        _factorial = _load_c_algebra_lib().factorial
        _factorial.argtypes = [ctypes.c_uint64]
        _factorial.restype = ctypes.c_uint64
    return _factorial(n)


def gcd(a: int, b: int) -> int:
    global _gcd
    if _gcd is None:
        _gcd = _load_c_algebra_lib().gcd
        _gcd.argtypes = [ctypes.c_uint64, ctypes.c_uint64]
        _gcd.restype = ctypes.c_uint64
    return _gcd(a, b)


def solve_diophantine(a: int, b: int, c: int) -> DiophantineSolution:
    global _solve_diophantine
    if _solve_diophantine is None:
        _solve_diophantine = _load_c_algebra_lib().solve_diophantine
        _solve_diophantine.argtypes = [ctypes.c_int64, ctypes.c_int64, ctypes.c_int64]
        _solve_diophantine.restype = DiophantineSolution
    return _solve_diophantine(a, b, c)
