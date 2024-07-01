package com.jpp_lab3.app.interfaces;

public interface IGaloisFieldFactory<T extends IGaloisField<T>> {
    T fromLong(final long value);
    T clone(final T instance);
}
