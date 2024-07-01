import pytest

from dh.common import GF1234567891, GF1234577
from dh.dh_setup import dh_setup_builder


DHSetup = dh_setup_builder(GF1234567891)


def test_is_compatible():
    sut =  DHSetup()

    assert sut.is_compatible(GF1234567891)
    assert not sut.is_compatible(GF1234577)


def test_power_exception():
    sut = DHSetup()

    base = GF1234567891(2)
    exponent = -3

    with pytest.raises(ValueError):
        sut.power(base, exponent)


@pytest.mark.parametrize(
    "base, exponent, expected",
    [(128, 0, 1), (2, 3, 8), (0, 5, 0), (GF1234567891.type_order() - 1, 2, 1)]
)
def test_power(base: int, exponent: int, expected: int):
    sut = DHSetup()

    assert sut.power(GF1234567891(base), exponent) == GF1234567891(expected)
