package com.jpp_lab3.app;

import com.jpp_lab3.app.interfaces.*;

import java.util.ArrayList;
import java.util.Random;

public class DHSetup<T extends IGaloisField<T>> {
    public DHSetup(IGaloisFieldFactory<T> tFactory) {
        this.tFactory = tFactory;

        final ArrayList<Long> primeDivisors =
            DHSetup.generatePrimeDivisors(this.tFactory.fromLong(0).order() - 1);
        do {
            this.generator = this.tFactory.fromLong(Math.max(Math.abs(DHSetup.random.nextLong()), 1));
        } while (!this.isValidGenerator(generator, primeDivisors));
    }

    public T getGenerator() {
        return this.generator;
    }

    public T power(T base, long exponent) throws IllegalArgumentException {
        if (exponent < 0)
            // We can but the exercise requires the exponent to be unsigned
            throw new IllegalArgumentException("Cannot perform exponentation with a negative exponent");

        T result = this.tFactory.fromLong(1);
        if (exponent == 0)
            return result;

        T baseCpy = this.tFactory.clone(base);
        while (exponent > 1) {
            if (exponent % 2 != 0) {
                result.multiply(baseCpy);
                exponent--;
            }

            baseCpy.multiply(baseCpy);
            exponent /= 2;
        }

        result.multiply(baseCpy);
        return result;
    }

    private boolean isValidGenerator(T candidate, final ArrayList<Long> primeDivisors) {
        final long orderMinus1 = candidate.order() - 1;

        for (long p : primeDivisors)
            if (this.power(candidate, orderMinus1 / p).toLong() == 1)
                return false;

        return true;
    }

    private IGaloisFieldFactory<T> tFactory;
    private T generator;

    private static ArrayList<Long> generatePrimeDivisors(long n) {
        ArrayList<Long> primeDivisors = new ArrayList<>();

        for (long i = 2; i * i <= n; i++) {
            if (n % i == 0) {
                primeDivisors.add(i);
                while (n % i == 0)
                    n /= i;
            }
        }

        return primeDivisors;
    }

    private static Random random = new Random(42);
}
