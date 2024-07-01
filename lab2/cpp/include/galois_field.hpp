#pragma once

#include <algorithm>
#include <cmath>
#include <concepts>
#include <cstdint>
#include <iostream>
#include <optional>
#include <ranges>
#include <stdexcept>

namespace jpp {

template <uint32_t N>
struct is_prime {
public:
    static constexpr bool value = is_prime<N>::_impl();

private:
    [[nodiscard]] static constexpr bool _impl() noexcept {
        if constexpr (N < 2u)
            return false;

        return std::ranges::all_of(
            std::views::iota(2u, static_cast<uint32_t>(std::sqrt(N)) + 1u),
            [](uint32_t i) { return N % i != 0u; }
        );
    }
};

template <uint32_t N>
inline constexpr bool is_prime_v = is_prime<N>::value;

template <uint32_t FieldOrder>
requires is_prime_v<FieldOrder>
class galois_field {
public:
    galois_field() = default;

    galois_field(const galois_field&) = default;
    galois_field(galois_field&&) = default;

    galois_field& operator=(const galois_field&) = default;
    galois_field& operator=(galois_field&&) = default;

    galois_field(const int64_t value) : _value(this->_mod(value)) {}

    ~galois_field() = default;

    [[nodiscard]] explicit operator int64_t() const noexcept {
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

    [[nodiscard]] uint32_t order() const noexcept {
        return FieldOrder;
    }

private:
    [[nodiscard]] static int64_t _mod(const int64_t value) noexcept {
        return value % FieldOrder;
    }

    [[nodiscard]] int64_t _inverse() const noexcept {
        int64_t t = 0, new_t = 1;
        int64_t r = FieldOrder, new_r = this->_value;

        while (new_r) {
            int64_t quotient = r / new_r;
            std::tie(t, new_t) = std::make_pair(new_t, t - quotient * new_t);
            std::tie(r, new_r) = std::make_pair(new_r, r - quotient * new_r);
        }

        return t < 0 ? t + FieldOrder : t;
    }

    int64_t _value = 0u;
};

} // namespace jpp
