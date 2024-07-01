#include "diffie_hellman.hpp"

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest.h>

using gf1234567891 = jpp::galois_field<1234567891>;
using dh_user_type = jpp::dh_user<gf1234567891>;
using dh_setup_type = jpp::dh_setup<gf1234567891>;

TEST_CASE("protocol users should correctly encrypt and decrypt messages") {
    dh_setup_type setup;

    dh_user_type alice(setup);
    dh_user_type bob(setup);

    alice.set_key(bob.public_key());
    bob.set_key(alice.public_key());

    gf1234567891 original_message = 519;

    REQUIRE_EQ(alice.decrypt(bob.encrypt(original_message)), original_message);
    REQUIRE_EQ(bob.decrypt(alice.encrypt(original_message)), original_message);
}
