namespace dh;

using dh.Interfaces;

public class DHUser<T> where T : IGaloisField<T> {
    public DHUser(IGaloisFieldFactory<T> tFactory, DHSetup<T> setup) {
        this.tFactory = tFactory;
        this.setup = setup;
        this.secret = Math.Max(DHUser<T>.randomULong(), 1UL);
        this.privKey = default(T);
    }

    public T GetPublicKey() {
        return this.setup.Power(this.setup.Generator, this.secret);
    }

    public void SetKey(T key) {
        this.privKey = this.setup.Power(key, this.secret);
    }

    public T Encrypt(T message) {
        if (this.privKey == null)
            throw new InvalidOperationException("Cannot encrypt a message without a private key");

        return message.Multiply(this.privKey!);
    }

    public T Decrypt(T code) {
        if (this.privKey == null)
            throw new InvalidOperationException("Cannot decrypt a code without a private key");

        return code.Divide(this.privKey!);
    }

    private static ulong randomULong() {
        byte[] buffer = new byte[8];
        DHUser<T>.random.NextBytes(buffer);
        return BitConverter.ToUInt64(buffer, 0);
    }

    private static Random random = new Random(42);

    private IGaloisFieldFactory<T> tFactory;
    private DHSetup<T> setup;
    private ulong secret;
    private T? privKey;
}
