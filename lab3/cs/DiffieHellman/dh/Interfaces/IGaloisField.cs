namespace dh.Interfaces;

using System;

public interface IGaloisField<T> : IBasicArithmetic<T> {
    long Value { get; }
    uint Order();
}
