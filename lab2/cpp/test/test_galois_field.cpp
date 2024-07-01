#include "galois_field.hpp"

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest.h>

#include <functional>
#include <vector>

using gf1234577 = jpp::galois_field<1234577u>;

TEST_CASE("GF elements should be comparable by value") {
    const gf1234577 a{5};
    const gf1234577 b{7};

    // operator==
    REQUIRE_EQ(a, a);
    REQUIRE_EQ(b, b);

    // operator!=
    REQUIRE_NE(a, b);
    REQUIRE_NE(b, a);

    // operator<, operator<=
    REQUIRE_LT(a, b);
    REQUIRE(a <= a);
    REQUIRE(a <= b);

    // operator>, operator>=
    REQUIRE_GT(b, a);
    REQUIRE(b >= a);
    REQUIRE(b >= b);
}

TEST_CASE("GF elements should be initialized mod(FieldOrder)") {
    const gf1234577 a;
    const gf1234577 b{a.order()};

    REQUIRE_EQ(a, b);
}

TEST_CASE("GF arithmetic operations should be performed mod(FieldOrder)") {
    struct arithmetic_operation_data {
        gf1234577 lhs;
        gf1234577 rhs;
        gf1234577 expected_result;
    };

    std::function<gf1234577(const gf1234577, const gf1234577)> operation;
    std::function<void(gf1234577&, const gf1234577)> modifier_operation;
    std::vector<arithmetic_operation_data> operation_data;

    constexpr uint32_t field_order = 1234577u;

    SUBCASE("addition") {
        operation = [](const gf1234577 lhs, const gf1234577 rhs) { return lhs + rhs; };
        modifier_operation =
            [](gf1234577& element, const gf1234577 modifier) { element += modifier; };
        operation_data = {
            {1, 3, 4},
            {0, field_order, 0},
            {10, field_order + 5, 15}
        };
    }

    SUBCASE("subtraction") {
        operation = [](const gf1234577 lhs, const gf1234577 rhs) { return lhs - rhs; };
        modifier_operation =
            [](gf1234577& element, const gf1234577 modifier) { element -= modifier; };
        operation_data = {
            {10, 3, 7},
            {0, 5, field_order - 5},
            {10, field_order + 5, 5}
        };
    }

    SUBCASE("multiplication") {
        operation = [](const gf1234577 lhs, const gf1234577 rhs) { return lhs * rhs; };
        modifier_operation =
            [](gf1234577& element, const gf1234577 modifier) { element *= modifier; };
        operation_data = {
            {3, 4, 12},
            {5, 6, 30},
            {2, (field_order + 3) / 2, 3}
        };
    }

    SUBCASE("division") {
        operation = [](const gf1234577 lhs, const gf1234577 rhs) { return lhs / rhs; };
        modifier_operation =
            [](gf1234577& element, const gf1234577 modifier) { element /= modifier; };
        operation_data = {
            {12, 4, 3},
            {30, 6, 5}
        };

        gf1234577 lhs{10};
        REQUIRE_THROWS_AS(operation(lhs, gf1234577{}), std::logic_error);
        REQUIRE_THROWS_AS(modifier_operation(lhs, gf1234577{}), std::logic_error);
    }

    CAPTURE(operation);
    CAPTURE(operation_data);
    CAPTURE(modifier_operation);

    for (const auto& data : operation_data)
        REQUIRE_EQ(operation(data.lhs, data.rhs), data.expected_result);

    for (auto& data : operation_data) {
        modifier_operation(data.lhs, data.rhs);
        REQUIRE_EQ(data.lhs, data.expected_result);
    }
}
