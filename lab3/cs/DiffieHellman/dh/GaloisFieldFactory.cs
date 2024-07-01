namespace dh;

using dh.Interfaces;

public class GaloisFieldFactory : IGaloisFieldFactory<GaloisField> {
    public GaloisField FromLong(long value) {
        return new GaloisField(value);
    }

    public GaloisField Clone(GaloisField instance) {
        return new GaloisField(instance);
    }
}
