from .common import GF1234567891, random_uint
from .interfaces.abstract_galois_field import AbstractGaloisField

from copy import deepcopy
from typing import Generic, Type, TypeVar


T = TypeVar("T", bound=AbstractGaloisField)


class DHSetupTmpl(Generic[T]):
    def __init__(self, key_type: Type[T]) -> None:
        self.key_type = key_type

        prime_divisors = DHSetupTmpl._generate_prime_divisors(self.key_type.type_order() - 1)
        found_valid_generator = False
        while not found_valid_generator:
            self._generator = self.key_type(max(random_uint(), 1))
            found_valid_generator = self._is_valid_generator(self._generator, prime_divisors)

    def is_compatible(self, key_type: Type[T]) -> bool:
        return self.key_type == key_type

    def generator(self) -> T:
        return self._generator

    def power(self, base: T, exponent: int) -> T:
        if exponent < 0:
            # We can but the exercise requires the exponent to be unsigned
            raise ValueError("Cannot perform exponentation with a negative exponent")

        result = self.key_type(1)
        if exponent == 0:
            return result

        base_cpy = deepcopy(base)
        while (exponent > 1):
            if exponent % 2 != 0:
                result *= base_cpy
                exponent -= 1

            base_cpy *= base_cpy
            exponent //= 2

        return result * base_cpy

    def _is_valid_generator(self, candidate: T, prime_divisors: list[int]) -> bool:
        order_minus_1 = candidate.order() - 1

        for p in prime_divisors:
            if int(self.power(candidate, order_minus_1 // p)) == 1:
                return False

        return True

    @staticmethod
    def _generate_prime_divisors(n: int) -> list[int]:
        prime_divisors = list()

        i = 2
        while i * i <= n:
            if n % i == 0:
                prime_divisors.append(i)
                while n % i == 0:
                    n //= i
            i += 1

        return prime_divisors


def dh_setup_builder(key_type: Type[T]):
    class DHSetup(DHSetupTmpl[key_type]):
        def __init__(self) -> None:
            super().__init__(key_type)

    return DHSetup
