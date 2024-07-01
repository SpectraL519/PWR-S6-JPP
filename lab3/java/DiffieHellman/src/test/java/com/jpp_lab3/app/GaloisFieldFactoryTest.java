package com.jpp_lab3.app;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class GaloisFieldFactoryTest {
    @BeforeAll
    public static void setUp() {
        sut = new GaloisFieldFactory();
    }


    @Test
    public void fromLongTest() {
        assertEquals(new GaloisField(gfValue), sut.fromLong(gfValue));
    }

    @Test
    public void cloneTest() {
        final GaloisField gf = new GaloisField(gfValue);
        assertEquals(gf, sut.clone(gf));
    }

    private static GaloisFieldFactory sut;

    private static final long gfValue = 519;
}
