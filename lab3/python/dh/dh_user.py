from .common import GF1234567891, random_uint
from .dh_setup import DHSetupTmpl
from .interfaces.abstract_galois_field import AbstractGaloisField

from copy import deepcopy
from typing import Generic, Protocol, Type, TypeVar


T = TypeVar("T", bound=AbstractGaloisField)


class DHUserTmpl(Generic[T]):
    def __init__(self, key_type: Type[T], setup: DHSetupTmpl[T]) -> None:
        if not setup.is_compatible(key_type):
            raise TypeError("Cannot instantiate DHUserTmpl[T] where type(setup) != DHSetupTmpl[T]")

        self.key_type = key_type
        self._setup = setup
        self._secret = max(random_uint(), 1)
        self._priv_key = None

    def is_compatible(self, key_type: Type[T]) -> bool:
        return self.key_type == key_type

    def public_key(self) -> T:
        return self._setup.power(self._setup.generator(), self._secret)

    def set_key(self, key: T) -> None:
        self._priv_key = self._setup.power(key, self._secret)

    def encrypt(self, message: T) -> T:
        if self._priv_key is None:
            raise ValueError("Cannot encrypt a message without a private key")

        return message * self._priv_key

    def decrypt(self, code: T) -> T:
        if self._priv_key is None:
            raise ValueError("Cannot decrypt a code without a private key")

        return code / self._priv_key


def dh_user_builder(key_type: Type[T]):
    class DHUser(DHUserTmpl[key_type]):
        def __init__(self, setup: DHSetupTmpl[key_type]) -> None:
            super().__init__(key_type, setup)

    return DHUser
