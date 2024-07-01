namespace dh;

using dh.Interfaces;

using System;

public class DHSetup<T> where T : IGaloisField<T> {
    public DHSetup(IGaloisFieldFactory<T> tFactory) {
        this.tFactory = tFactory;

        List<long> primeDivisors = DHSetup<T>.generatePrimeDivisors(this.tFactory.FromLong(0).Order() - 1);
        do {
            long randomNumber = Math.Abs(DHSetup<T>.randomLong());
            this.generator = this.tFactory.FromLong(randomNumber > 0 ? randomNumber : 1);
        } while (!this.isValidGenerator(this.generator, primeDivisors));
    }

    public T Generator {
        get => this.generator;
    }

    public T Power(T gfBase, ulong exponent) {
        T result = this.tFactory.FromLong(1);
        if (exponent == 0)
            return result;

        T baseCpy = this.tFactory.Clone(gfBase);
        while (exponent > 1) {
            if (exponent % 2 != 0) {
                result = result.Multiply(baseCpy);
                exponent--;
            }

            baseCpy = baseCpy.Multiply(baseCpy);
            exponent /= 2;
        }

        return result.Multiply(baseCpy);
    }

    private bool isValidGenerator(T candidate, List<long> primeDivisors) {
        long orderMinus1 = candidate.Order() - 1;

        foreach (long p in primeDivisors)
            if (this.Power(candidate, (ulong)(orderMinus1 / p)).Value == 1)
                return false;

        return true;
    }

    private IGaloisFieldFactory<T> tFactory;
    private T generator;

    private static List<long> generatePrimeDivisors(long n) {
        List<long> primeDivisors = new List<long>();

        for (long i = 2; i * i <= n; i++) {
            if (n % i == 0) {
                primeDivisors.Add(i);
                while (n % i == 0)
                    n /= i;
            }
        }

        return primeDivisors;
    }

    private static long randomLong() {
        byte[] buffer = new byte[8];
        DHSetup<T>.random.NextBytes(buffer);
        return BitConverter.ToInt64(buffer, 0);
    }

    private static Random random = new Random(42);
}
