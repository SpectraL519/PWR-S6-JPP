import math

def galois_field_builder(field_order: int):
    if not isinstance(field_order, int) or field_order < 2:
        raise ValueError("Parameter must be a field_order number greater than 1")

    if not all(field_order % i for i in range(2, math.isqrt(field_order) + 1)):
        raise ValueError(f"{field_order} is not a prime number")

    class GaloisField:
        def __init__(self, value = 0):
            self._value = self._mod(value)

        def order(self):
            return field_order

        def __int__(self):
            return self._value

        def __bool__(self):
            return bool(self._value)

        def __eq__(self, other):
            return self._value == other._value

        def __lt__(self, other):
            return self._value < other._value

        def __le__(self, other):
            return self._value <= other._value

        def __gt__(self, other):
            return self._value > other._value

        def __ge__(self, other):
            return self._value >= other._value

        def __add__(self, other):
            return GaloisField(self._value + other._value)

        def __sub__(self, other):
            return GaloisField(field_order + (self._value - other._value) % field_order)

        def __mul__(self, other):
            return GaloisField(self._value * other._value)

        def __truediv__(self, other):
            if not other:
                raise ValueError(f"Cannot invert 0 mod({field_order})")
            return GaloisField(self._value * other._inverse())

        def __repr__(self):
            return str(self._value)

        def __str__(self):
            return str(self._value)

        def _mod(self, value):
            return value % field_order

        def _inverse(self):
            t, new_t = 0, 1
            r, new_r = field_order, self._value

            while new_r:
                quotient = r // new_r
                t, new_t = new_t, t - quotient * new_t
                r, new_r = new_r, r - quotient * new_r

            return t + field_order if t < 0 else t

    return GaloisField
