package com.jpp_lab3.app;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.params.provider.Arguments.arguments;


import java.util.stream.Stream;

public class GaloisFieldTest {
    @Test
    public void elementsShouldBeComparableByValue() {
        GaloisField a = new GaloisField(5);
        GaloisField b = new GaloisField(7);

        assertEquals(a, a);
        assertEquals(b, b);

        assertNotEquals(a, b);
        assertNotEquals(b, a);

        assertTrue(a.compareTo(a) == 0);
        assertTrue(b.compareTo(b) == 0);

        assertTrue(a.compareTo(b) < 0);
        assertTrue(b.compareTo(a) > 0);
    }

    @Test
    public void elementsShouldBeInitializedModFieldOrder() {
        assertEquals(new GaloisField(), new GaloisField(GaloisField.ORDER));
    }

    static Stream<Arguments> additionData() {
        return Stream.of(
            arguments(1, 3, 4),
            arguments(0, GaloisField.ORDER, 0),
            arguments(10, GaloisField.ORDER + 5, 15)
        );
    }

    @ParameterizedTest
    @MethodSource("additionData")
    public void additionShouldBePerformedModFieldOrder(
        long lhsValue, long rhsValue, long expectedValue
    ) {
        GaloisField lhs = new GaloisField(lhsValue);
        GaloisField rhs = new GaloisField(rhsValue);
        GaloisField expected = new GaloisField(expectedValue);

        GaloisField result = GaloisField.add(lhs, rhs);
        assertEquals(expected, result);

        lhs.add(rhs);
        assertEquals(expected, lhs);
    }

    static Stream<Arguments> subtractionData() {
        return Stream.of(
            arguments(10, 3, 7),
            arguments(0, 5, GaloisField.ORDER - 5),
            arguments(10, GaloisField.ORDER + 5, 5)
        );
    }

    @ParameterizedTest
    @MethodSource("subtractionData")
    public void subtractionShouldBePerformedModFieldOrder(
        long lhsValue, long rhsValue, long expectedValue
    ) {
        GaloisField lhs = new GaloisField(lhsValue);
        GaloisField rhs = new GaloisField(rhsValue);
        GaloisField expected = new GaloisField(expectedValue);

        GaloisField result = GaloisField.subtract(lhs, rhs);
        assertEquals(expected, result);

        lhs.subtract(rhs);
        assertEquals(expected, lhs);
    }

    static Stream<Arguments> multiplicationData() {
        return Stream.of(
            arguments(3, 4, 12),
            arguments(5, 6, 30),
            arguments(2, (GaloisField.ORDER + 3) / 2, 3)
        );
    }

    @ParameterizedTest
    @MethodSource("multiplicationData")
    public void multiplicationShouldBePerformedModFieldOrder(
        long lhsValue, long rhsValue, long expectedValue
    ) {
        GaloisField lhs = new GaloisField(lhsValue);
        GaloisField rhs = new GaloisField(rhsValue);
        GaloisField expected = new GaloisField(expectedValue);

        GaloisField result = GaloisField.multiply(lhs, rhs);
        assertEquals(expected, result);

        lhs.multiply(rhs);
        assertEquals(expected, lhs);
    }

    static Stream<Arguments> divisionData() {
        return Stream.of(
            arguments(12, 4, 3),
            arguments(30, 6, 5)
        );
    }

    @ParameterizedTest
    @MethodSource("divisionData")
    public void divisionShouldBePerformedModFieldOrder(
        long lhsValue, long rhsValue, long expectedValue
    ) {
        GaloisField lhs = new GaloisField(lhsValue);
        GaloisField rhs = new GaloisField(rhsValue);
        GaloisField expected = new GaloisField(expectedValue);

        GaloisField result = GaloisField.divide(lhs, rhs);
        assertEquals(expected, result);

        lhs.divide(rhs);
        assertEquals(expected, lhs);
    }

    @Test
    public void divisionByZeroShouldThrowAnException() {
        GaloisField lhs = new GaloisField(10);
        GaloisField rhs = new GaloisField(0);

        assertThrows(ArithmeticException.class, () -> { GaloisField.divide(lhs, rhs); });
        assertThrows(ArithmeticException.class, () -> { lhs.divide(rhs); });
    }
}
