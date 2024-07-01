package com.jpp_lab2.app;

import java.util.Objects;

public class GaloisField implements Comparable<GaloisField> {
    GaloisField() {}

    GaloisField(long value) {
        this.value = GaloisField.mod(value);
    }

    GaloisField(final GaloisField other) {
        this.value = other.value;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;

        if (obj == null || getClass() != obj.getClass())
            return false;

        GaloisField other = (GaloisField)obj;
        return this.value == other.value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.value);
    }

    @Override
    public int compareTo(GaloisField other) {
        return Long.compare(this.value, other.value);
    }

    public static GaloisField add(final GaloisField lhs, final GaloisField rhs) {
        return new GaloisField(lhs.value + rhs.value);
    }

    public static GaloisField subtract(final GaloisField lhs, final GaloisField rhs) {
        return new GaloisField(GaloisField.ORDER + GaloisField.mod(lhs.value - rhs.value));
    }

    public static GaloisField multiply(final GaloisField lhs, final GaloisField rhs) {
        return new GaloisField(lhs.value * rhs.value);
    }

    public static GaloisField divide(
        final GaloisField lhs, final GaloisField rhs
    ) throws ArithmeticException {
        if (rhs.value == 0)
            throw new ArithmeticException(String.format("Cannot invert 0 mod(%d)", GaloisField.ORDER));

        return new GaloisField(lhs.value * rhs.inverse());
    }

    public void add(final GaloisField other) {
        this.value = GaloisField.mod(this.value + other.value);
    }

    public void subtract(final GaloisField other) {
        this.value = GaloisField.mod(GaloisField.ORDER + GaloisField.mod(this.value - other.value));
    }

    public void multiply(final GaloisField other) {
        this.value = GaloisField.mod(this.value * other.value);
    }

    public void divide(final GaloisField other) {
        if (other.value == 0)
            throw new ArithmeticException(String.format("Cannot invert 0 mod(%d)", GaloisField.ORDER));

        this.value = GaloisField.mod(this.value * other.inverse());
    }

    public long order() {
        return GaloisField.ORDER;
    }

    @Override
    public String toString() {
        return String.valueOf(this.value);
    }

    public long toLong() {
        return this.value;
    }

    private static long mod(long value) {
        return value % GaloisField.ORDER;
    }

    private long inverse() {
        long t = 0, newT = 1;
        long r = GaloisField.ORDER, newR = value;

        while (newR != 0) {
            long quotient = r / newR;
            long temp = t;
            t = newT;
            newT = temp - quotient * newT;

            temp = r;
            r = newR;
            newR = temp - quotient * newR;
        }

        return t < 0 ? (long)(t + GaloisField.ORDER) : (long)t;
    }

    public static final long ORDER = 1234577;
    private long value;
}
