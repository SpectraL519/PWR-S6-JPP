#pragma once

#include "galois_field.hpp"
#include "utility.hpp"

#include <optional>
#include <random>
#include <stdexcept>
#include <vector>

#define RSEED 42

namespace jpp {

template <c_galois_field<uint32_t> T>
class dh_setup {
public:
    using key_type = T;

    dh_setup() {
        static std::mt19937 gen32 = this->_prepare_random_number_generator();
        static std::uniform_int_distribution<uint32_t> uniform{1, key_type::order() - 1};

        do {
            this->_generator = key_type{uniform(gen32)};
        } while (not dh_setup::_is_valid_generator(this->_generator));
    }

    dh_setup(const dh_setup&) = default;
    dh_setup(dh_setup&&) = default;

    dh_setup& operator=(const dh_setup&) = default;
    dh_setup& operator=(dh_setup&&) = default;

    ~dh_setup() = default;

    [[nodiscard]] key_type generator() const noexcept {
        return this->_generator;
    }

    [[nodiscard]] static key_type power(key_type base, uint64_t exponent) noexcept {
        key_type result = 1;
        if (exponent == 0)
            return result;

        while (exponent > 1) {
            if (exponent % 2 != 0) {
                result *= base;
                exponent--;
            }

            base *= base;
            exponent /= 2;
        }

        return result * base;
    }

private:
    [[nodiscard]] static std::mt19937 _prepare_random_number_generator() noexcept {
        std::mt19937 gen32;
        gen32.seed(RSEED);
        return gen32;
    }

    [[nodiscard]] static std::vector<uint32_t> _generate_prime_divisors(uint32_t n) noexcept {
        std::vector<uint32_t> prime_divisors;

        for (uint32_t i = 2; i * i <= n; i++) {
            if (n % i == 0) {
                prime_divisors.push_back(i);
                while (n % i == 0)
                    n /= i;
            }
        }

        return prime_divisors;
    }

    [[nodiscard]] static bool _is_valid_generator(const key_type& candidate) noexcept {
        static const auto order_minus_1 = key_type::order() - 1;
        static const auto prime_divisors = dh_setup::_generate_prime_divisors(order_minus_1);

        for (const auto p : prime_divisors)
            if (dh_setup::power(candidate, order_minus_1 / p) == key_type{1u})
                return false;

        return true;
    }

    key_type _generator;
};

template <c_galois_field<uint32_t> T>
class dh_user {
public:
    using key_type = T;
    using setup_type = dh_setup<key_type>;

    dh_user() = delete;

    dh_user(const setup_type& setup) : _setup(setup) {
        static std::mt19937_64 gen64 = this->_prepare_random_number_generator();
        static std::uniform_int_distribution<uint64_t> uniform{1};

        this->_secret = uniform(gen64);
    }

    dh_user(const dh_user&) = default;
    dh_user(dh_user&&) = default;

    dh_user& operator=(const dh_user&) = default;
    dh_user& operator=(dh_user&&) = default;

    ~dh_user() = default;

    [[nodiscard]] key_type public_key() const noexcept {
        return setup_type::power(this->_setup.generator(), this->_secret);
    }

    void set_key(const key_type& key) noexcept {
        this->_priv_key = setup_type::power(key, this->_secret);
    }

    [[nodiscard]] key_type encrypt(const key_type& message) const {
        if (not this->_priv_key)
            throw std::logic_error("Cannot encrypt a message without a private key");

        return message * this->_priv_key.value();
    }

    [[nodiscard]] key_type decrypt(const key_type& code) const {
        if (not this->_priv_key)
            throw std::logic_error("Cannot decrypt a code without a private key");

        return code / this->_priv_key.value();
    }

private:
    [[nodiscard]] static std::mt19937_64 _prepare_random_number_generator() noexcept {
        std::mt19937_64 gen64;
        gen64.seed(RSEED);
        return gen64;
    }

    const setup_type& _setup;
    uint64_t _secret;
    std::optional<key_type> _priv_key;
};

} // namespace jpp
