package com.jpp_lab3.app;

import com.jpp_lab3.app.interfaces.*;

import java.util.Optional;
import java.util.Random;

public class DHUser<T extends IGaloisField<T>> {
    DHUser(IGaloisFieldFactory<T> tFactory, DHSetup<T> setup) {
        this.tFactory = tFactory;
        this.setup = setup;
        this.secret = Math.max(Math.abs(DHUser.random.nextLong()), 1);
        this.privKey = Optional.empty();
    }

    public T getPublicKey() {
        return this.setup.power(this.setup.getGenerator(), this.secret);
    }

    void setKey(T key) {
        this.privKey = Optional.of(this.setup.power(key, this.secret));
    }

    public T encrypt(T message) throws IllegalStateException {
        if (!this.privKey.isPresent())
            throw new IllegalStateException("Cannot encrypt a message without a private key");

        T encryptedMessage = this.tFactory.clone(message);
        encryptedMessage.multiply(this.privKey.get());
        return encryptedMessage;
    }

    public T decrypt(T message) throws IllegalStateException {
        if (!this.privKey.isPresent())
            throw new IllegalStateException("Cannot decrypt a code without a private key");

        T decryptedMessage = this.tFactory.clone(message);
        decryptedMessage.divide(this.privKey.get());
        return decryptedMessage;
    }

    private static Random random = new Random(42);

    private IGaloisFieldFactory<T> tFactory;
    private DHSetup<T> setup;
    private long secret;
    private Optional<T> privKey;
}
