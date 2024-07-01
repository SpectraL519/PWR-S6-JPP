from .galois_field import galois_field_builder

import random
import sys


GF1234577 = galois_field_builder(1234577)
GF1234567891 = galois_field_builder(1234567891)


def random_uint():
    return random.randint(0, 10 ** random.randint(0, len(str(sys.maxsize))) - 1)
