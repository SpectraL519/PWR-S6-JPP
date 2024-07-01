import pytest

from dh.common import GF1234567891
from dh.dh_setup import dh_setup_builder
from dh.dh_user import dh_user_builder


DHSetup = dh_setup_builder(GF1234567891)
DHUser = dh_user_builder(GF1234567891)


def test_diffie_hellman_protocol():
    setup = DHSetup()

    alice = DHUser(setup)
    bob = DHUser(setup)

    alice.set_key(bob.public_key())
    bob.set_key(alice.public_key())

    original_message = GF1234567891(519)

    assert alice.decrypt(bob.encrypt(original_message)) == original_message
    assert bob.decrypt(alice.encrypt(original_message)) == original_message
