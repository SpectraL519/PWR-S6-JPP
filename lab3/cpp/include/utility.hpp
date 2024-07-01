#pragma once

#include <algorithm>
#include <cmath>
#include <compare>
#include <concepts>
#include <ranges>

namespace jpp {

template <std::unsigned_integral T, T N>
struct is_prime {
public:
    static constexpr bool value = is_prime<T, N>::_impl();

private:
    [[nodiscard]] static constexpr bool _impl() noexcept {
        if constexpr (N < 2u)
            return false;

        return std::ranges::all_of(
            std::views::iota(2u, static_cast<T>(std::sqrt(N)) + 1u),
            [](const T i) { return N % i != 0u; }
        );
    }
};

template <std::unsigned_integral T, T N>
inline constexpr bool is_prime_v = is_prime<T, N>::value;

template <typename T, typename U>
concept unsigned_compatible =
    std::unsigned_integral<U> and std::constructible_from<const U> and std::convertible_to<T, U>;

template <typename T>
concept comparable = requires(const T lhs, const T rhs) {
    { lhs <=> rhs } -> std::convertible_to<std::weak_ordering>;
    { lhs == rhs } -> std::convertible_to<bool>;
};

template <typename T>
concept basic_arithmetic = requires(const T a, const T b, T c) {
    { a + b } -> std::same_as<T>;
    { a - b } -> std::same_as<T>;
    { a * b } -> std::same_as<T>;
    { a / b } -> std::same_as<T>;
    { c += a } -> std::same_as<T&>;
    { c -= a } -> std::same_as<T&>;
    { c *= a } -> std::same_as<T&>;
    { c /= a } -> std::same_as<T&>;
};

template <typename T, typename U>
concept c_galois_field =
    std::copy_constructible<T> and unsigned_compatible<T, U> and comparable<T> and basic_arithmetic<T> and
    requires { { T::order() } -> std::convertible_to<U>; };

} // namespace jpp
