#pragma once

#include "mw_int.hpp"
#include "utility.hpp"

#include <algorithm>
#include <cstdint>
#include <iostream>
#include <stdexcept>

namespace jpp {

template <uint32_t FieldOrder>
requires is_prime_v<uint32_t, FieldOrder>
class galois_field {
public:
    using value_type = mw_int_t;

    galois_field() = default;

    galois_field(const galois_field&) = default;
    galois_field(galois_field&&) = default;

    galois_field& operator=(const galois_field&) = default;
    galois_field& operator=(galois_field&&) = default;

    // template <std::integral T>
    galois_field(const value_type value) : _value(this->_mod(value)) {}

    ~galois_field() = default;

    [[nodiscard]] explicit operator value_type() const noexcept {
        return this->_value;
    }

    [[nodiscard]] operator bool() const noexcept {
        return static_cast<bool>(this->_value);
    }

    [[nodiscard]] auto operator<=>(const galois_field& other) const noexcept = default;

    [[nodiscard]] friend galois_field operator+(const galois_field lhs, const galois_field rhs) noexcept {
        return galois_field{lhs._value + rhs._value};
    }

    [[nodiscard]] friend galois_field operator-(const galois_field lhs, const galois_field rhs) noexcept {
        return galois_field{FieldOrder + (lhs._value - rhs._value) % FieldOrder};
    }

    [[nodiscard]] friend galois_field operator*(const galois_field lhs, const galois_field rhs) noexcept {
        return galois_field{lhs._value * rhs._value};
    }

    [[nodiscard]] friend galois_field operator/(const galois_field lhs, const galois_field rhs) {
        if (not rhs)
            throw std::logic_error{"Cannot invert 0 mod(" + std::to_string(FieldOrder) + ")"};

        return galois_field{lhs._value * rhs._inverse()};
    }

    galois_field& operator+=(const galois_field& other) noexcept {
        *this = *this + other;
        return *this;
    }

    galois_field& operator-=(const galois_field& other) noexcept {
        *this = *this - other;
        return *this;
    }

    galois_field& operator*=(const galois_field& other) noexcept {
        *this = *this * other;
        return *this;
    }

    galois_field& operator/=(const galois_field& other) {
        *this = *this / other;
        return *this;
    }

    friend std::ostream& operator<<(std::ostream& os, const galois_field gf) noexcept {
        os << gf._value;
        return os;
    }

    friend std::istream& operator>>(std::istream& is, galois_field& gf) noexcept {
        is >> gf._value;
        gf._value = gf._mod(gf._value);
        return is;
    }

    [[nodiscard]] static constexpr uint32_t order() {
        return FieldOrder;
    }

private:
    [[nodiscard]] static value_type _mod(const value_type value) noexcept {
        return value % FieldOrder;
    }

    [[nodiscard]] value_type _inverse() const noexcept {
        value_type t = 0, new_t = 1;
        value_type r = FieldOrder, new_r = this->_value;

        while (new_r) {
            value_type quotient = r / new_r;
            std::tie(t, new_t) = std::make_pair(new_t, t - quotient * new_t);
            std::tie(r, new_r) = std::make_pair(new_r, r - quotient * new_r);
        }

        return t < 0 ? t + FieldOrder : t;
    }

    value_type _value = 0u;
};

} // namespace jpp
