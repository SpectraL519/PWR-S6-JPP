import pytest

from dh.galois_field import galois_field_builder
from dh.common import GF1234577


def test_galois_field_builder():
    '''
    GF builder should throw if value is not a prime number
    '''
    with pytest.raises(ValueError):
        _ = galois_field_builder(12)


def test_comparison():
    '''
    GF elements should be comparable by value
    '''
    a = GF1234577(5)
    b = GF1234577(7)

    assert a == a
    assert b == b

    assert a != b
    assert b != a

    assert a < b
    assert a <= a
    assert a <= b

    assert b > a
    assert b >= a
    assert b >= b


def test_initialization():
    '''
    * GF elements should be initialized mod(field_order)
    '''

    a = GF1234577()
    b = GF1234577(a.order())

    assert a == b


@pytest.mark.parametrize(
    "lhs, rhs, expected",
    [(1, 3, 4), (0, 1234577, 0), (10, 1234577 + 5, 15)]
)
def test_addition(lhs: int, rhs: int, expected: int):
    '''
    GF addition should be performed mod(field_order)
    '''

    assert GF1234577(lhs) + GF1234577(rhs) == GF1234577(expected)


@pytest.mark.parametrize(
    "lhs, rhs, expected",
    [(10, 3, 7), (0, 5, 1234577 - 5), (10, 1234577 + 5, 5)]
)
def test_subtraction(lhs: int, rhs: int, expected: int):
    '''
    GF subtraction should be performed mod(field_order)
    '''
    assert GF1234577(lhs) - GF1234577(rhs) == GF1234577(expected)


@pytest.mark.parametrize(
    "lhs, rhs, expected",
    [(3, 4, 12), (5, 6, 30), (2, (1234577 + 3) // 2, 3)]
)
def test_multiplication(lhs: int, rhs: int, expected: int):
    '''
    GF multiplication should be performed mod(field_order)
    '''

    assert GF1234577(lhs) * GF1234577(rhs) == GF1234577(expected)


def test_zero_division():
    '''
    GF zero division shoudl throw an error
    '''

    with pytest.raises(ValueError):
        GF1234577(10) / GF1234577(0)


@pytest.mark.parametrize(
    "lhs, rhs, expected",
    [(12, 4, 3), (30, 6, 5)]
)
def test_division(lhs: int, rhs: int, expected: int):
    '''
    GF division should be performed mod(field_order)
    '''

    assert GF1234577(lhs) / GF1234577(rhs) == GF1234577(expected)
