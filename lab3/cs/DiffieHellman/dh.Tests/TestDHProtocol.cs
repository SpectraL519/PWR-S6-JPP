namespace dh.Tests;

[TestClass]
public class TestDHProtocl {
    [TestMethod]
    public void TestDiffieHellmanProtocol() {
        GaloisFieldFactory gfFactory = new GaloisFieldFactory();
        DHSetup<GaloisField> setup = new DHSetup<GaloisField>(gfFactory);

        DHUser<GaloisField> alice = new DHUser<GaloisField>(gfFactory, setup);
        DHUser<GaloisField> bob = new DHUser<GaloisField>(gfFactory, setup);

        alice.SetKey(bob.GetPublicKey());
        bob.SetKey(alice.GetPublicKey());

        GaloisField originalMessage = gfFactory.FromLong(519);

        Assert.AreEqual(alice.Decrypt(bob.Encrypt(originalMessage)), originalMessage);
        Assert.AreEqual(bob.Decrypt(alice.Encrypt(originalMessage)), originalMessage);
    }
}
