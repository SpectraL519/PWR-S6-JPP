#pragma once

#include <iostream>
#include <cstdint>

#ifdef __SIZEOF_INT128__

using mw_int_t = __int128_t;

std::istream& operator>>(std::istream& is, __int128_t& value) {
    value = 0;
    bool negative = false;
    std::streambuf* sb = is.rdbuf();

    // Skip whitespace
    char c;
    do {
        c = static_cast<char>(sb->sbumpc());
    } while (std::isspace(c, is.getloc()));

    // Check for sign
    if (c == '-') {
        negative = true;
        c = static_cast<char>(sb->sbumpc());
    }
    else if (c == '+') {
        c = static_cast<char>(sb->sbumpc());
    }

    // Process digits
    bool success = false;
    while (std::isdigit(c, is.getloc())) {
        success = true;
        value = value * 10 + (c - '0');
        c = static_cast<char>(sb->sbumpc());
    }

    // If no valid digits were read, fail
    if (not success) {
        is.setstate(std::ios_base::failbit);
        return is;
    }

    // Apply sign if necessary
    if (negative)
        value = -value;

    // Put back the non-digit character
    if (sb->sputbackc(c) == std::char_traits<char>::eof())
        is.setstate(std::ios_base::failbit);

    return is;
}

std::ostream& operator<<(std::ostream& os, __int128_t value) {
    static constexpr short int64_bit_length = 64;

    if (value < 0) {
        os << "-";
        value = -value;
    }

    const auto first_64_bits = static_cast<int64_t>(value >> int64_bit_length);
    if (first_64_bits)
        os << first_64_bits;
    os << static_cast<int64_t>(value);

    return os;
}

#else

using mw_int_t = int64_t;

#endif
