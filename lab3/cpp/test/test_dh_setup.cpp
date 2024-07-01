#include "diffie_hellman.hpp"

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest.h>

#include <vector>

using gf1234567891 = jpp::galois_field<1234567891>;
using sut_type = jpp::dh_setup<gf1234567891>;

TEST_CASE("power should correctly calculate the exponentation expression in the galois_field") {
    struct exponentation_data {
        gf1234567891 key;
        uint64_t exponent;
        gf1234567891 expected_result;
    };

    std::vector<exponentation_data> test_data = {
        {gf1234567891{128}, 0ull, gf1234567891{1}},
        {gf1234567891{2}, 3ull, gf1234567891{8}},
        {gf1234567891{}, 5ull, gf1234567891{}},
        {gf1234567891{gf1234567891::order() - 1}, 2ull, gf1234567891{1}}
    };

    for (const auto& data : test_data)
        REQUIRE_EQ(sut_type::power(data.key, data.exponent), data.expected_result);
}
