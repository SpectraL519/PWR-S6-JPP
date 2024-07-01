package com.jpp_lab3.app.interfaces;

public interface IGaloisField<T> extends Comparable<T>, Cloneable {
    long toLong();
    long order();

    void add(final T other);
    void subtract(final T other);
    void multiply(final T other);
    void divide(final T other);
}
