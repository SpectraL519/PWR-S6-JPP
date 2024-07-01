#include "diffie_hellman.hpp"

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest.h>

#include <tuple>

using gf1234567891 = jpp::galois_field<1234567891>;
using sut_type = jpp::dh_user<gf1234567891>;
using dh_setup_type = jpp::dh_setup<gf1234567891>;

namespace {

dh_setup_type setup;

gf1234567891 key = 111;
gf1234567891 message = 123;
gf1234567891 code = 321;

} // namespace

TEST_CASE("encrypt should throw if the private key has not been set") {
    sut_type sut(setup);
    REQUIRE_THROWS_AS(std::ignore = sut.encrypt(message), std::logic_error);
}

TEST_CASE("encrypt should return an encrypted message if a private key is set") {
    sut_type sut(setup);
    sut.set_key(key);

    REQUIRE_NOTHROW(std::ignore = sut.encrypt(message));
}

TEST_CASE("decrypt should throw if the private key has not been set") {
    sut_type sut(setup);
    REQUIRE_THROWS_AS(std::ignore = sut.decrypt(code), std::logic_error);
}

TEST_CASE("decrypt should return an encrypted message if a private key is set") {
    sut_type sut(setup);
    sut.set_key(key);

    REQUIRE_NOTHROW(std::ignore = sut.decrypt(code));
}
