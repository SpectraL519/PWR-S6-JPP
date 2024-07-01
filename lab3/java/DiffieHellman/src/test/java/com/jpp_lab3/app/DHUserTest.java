package com.jpp_lab3.app;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class DHUserTest {
    @Test
    public void encryptExceptionTest() {
        assertThrows(IllegalStateException.class, () -> { sut.encrypt(message); });
    }

    @Test
    public void encryptTest() {
        sut.setKey(key);
        assertDoesNotThrow(() -> { sut.encrypt(message); });
    }

    @Test
    public void decryptExceptionTest() {
        assertThrows(IllegalStateException.class, () -> { sut.decrypt(code); });
    }

    @Test
    public void decryptTest() {
        sut.setKey(key);
        assertDoesNotThrow(() -> { sut.decrypt(message); });
    }

    private DHUser<GaloisField> sut = new DHUser<>(gfFactory, setup);

    private static GaloisFieldFactory gfFactory = new GaloisFieldFactory();
    private static DHSetup<GaloisField> setup = new DHSetup<>(gfFactory);
    private static GaloisField key = new GaloisField(111);
    private static GaloisField message = new GaloisField(123);
    private static GaloisField code = new GaloisField(321);
}
