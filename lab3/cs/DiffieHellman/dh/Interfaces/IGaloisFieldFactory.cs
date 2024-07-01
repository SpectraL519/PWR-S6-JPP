namespace dh.Interfaces;

using System;

public interface IGaloisFieldFactory<T> {
    T FromLong(long value);
    T Clone(T source);
}
