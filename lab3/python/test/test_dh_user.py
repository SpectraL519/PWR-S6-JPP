import pytest

from dh.common import GF1234567891, GF1234577
from dh.dh_setup import dh_setup_builder
from dh.dh_user import dh_user_builder


DHSetup = dh_setup_builder(GF1234567891)
DHUser = dh_user_builder(GF1234567891)


def test_initialization_exception():
    InvalidDHSetup = dh_setup_builder(GF1234577)

    with pytest.raises(TypeError):
        _ = DHUser(InvalidDHSetup())


setup = DHSetup()
key = GF1234567891(111)
message = GF1234567891(123)
code = GF1234567891(321)


def test_is_compatible():
    sut = DHUser(setup)

    assert sut.is_compatible(GF1234567891)
    assert not sut.is_compatible(GF1234577)


def test_encrypt_exception():
    sut = DHUser(setup)

    with pytest.raises(ValueError):
        _ = sut.encrypt(message)


def test_encrypt():
    sut = DHUser(setup)
    sut.set_key(key)

    assert isinstance(sut.encrypt(message), GF1234567891)


def test_decrypt_exception():
    sut = DHUser(setup)

    with pytest.raises(ValueError):
        _ = sut.decrypt(code)


def test_decrypt():
    sut = DHUser(setup)
    sut.set_key(key)

    assert isinstance(sut.decrypt(code), GF1234567891)
