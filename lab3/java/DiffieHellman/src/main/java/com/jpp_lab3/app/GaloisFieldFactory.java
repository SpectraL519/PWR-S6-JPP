package com.jpp_lab3.app;

import com.jpp_lab3.app.interfaces.IGaloisFieldFactory;

public class GaloisFieldFactory implements IGaloisFieldFactory<GaloisField> {
    @Override
    public GaloisField fromLong(final long value) {
        return new GaloisField(value);
    }

    @Override
    public GaloisField clone(final GaloisField instance) {
        return new GaloisField(instance);
    }
}
