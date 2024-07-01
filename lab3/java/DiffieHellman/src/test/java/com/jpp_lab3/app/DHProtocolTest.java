package com.jpp_lab3.app;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class DHProtocolTest {
    @Test
    public void diffieHellmanProtocolTest() {
        GaloisFieldFactory gfFactory = new GaloisFieldFactory();
        DHSetup<GaloisField> setup = new DHSetup<>(gfFactory);

        DHUser<GaloisField> alice = new DHUser<>(gfFactory, setup);
        DHUser<GaloisField> bob = new DHUser<>(gfFactory, setup);

        alice.setKey(bob.getPublicKey());
        bob.setKey(alice.getPublicKey());

        GaloisField originalMessage = gfFactory.fromLong(519);

        assertEquals(originalMessage, alice.decrypt(bob.encrypt(originalMessage)));
        assertEquals(originalMessage, bob.decrypt(alice.encrypt(originalMessage)));
    }
}
