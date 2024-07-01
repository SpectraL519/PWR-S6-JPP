package com.jpp_lab3.app;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.params.provider.Arguments.arguments;

import java.util.stream.Stream;

public class DHSetupTest {
    @Test
    public void powerExceptionTest() {
        GaloisField base = new GaloisField(2);
        long exponent = -3;

        assertThrows(IllegalArgumentException.class, () -> { sut.power(base, exponent); });
    }

    static Stream<Arguments> powerTestData() {
        return Stream.of(
            arguments(128, 0, 1),
            arguments(2, 3, 8),
            arguments(0, 5, 0),
            arguments(GaloisField.ORDER - 1, 2, 1)
        );
    }

    @ParameterizedTest
    @MethodSource("powerTestData")
    public void powerTest(long baseValue, long exponent, long expectedValue) {
        GaloisField base = new GaloisField(baseValue);
        GaloisField expected = new GaloisField(expectedValue);

        assertEquals(expected, sut.power(base, exponent));
    }

    private DHSetup<GaloisField> sut = new DHSetup<>(new GaloisFieldFactory());
}
