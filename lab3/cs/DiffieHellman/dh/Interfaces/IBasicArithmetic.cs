namespace dh.Interfaces;

public interface IBasicArithmetic<T> {
    T Add(T other);
    T Subtract(T other);
    T Multiply(T other);
    T Divide(T other);
}
