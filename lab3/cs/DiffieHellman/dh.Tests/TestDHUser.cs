namespace dh.Tests;

using dh.Tests.Assertions;

[TestClass]
public class TestDHUser {
    [TestMethod]
    public void TestEncryptException() {
        Assert.ThrowsException<InvalidOperationException>(() => { sut.Encrypt(message); });
    }

    [TestMethod]
    public void TestEncrypt() {
        sut.SetKey(key);
        CustomAssertions.AssertDoesNotThrow(() => { sut.Encrypt(message); });
    }

    [TestMethod]
    public void TestDecryptException() {
        Assert.ThrowsException<InvalidOperationException>(() => { sut.Decrypt(code); });
    }

    [TestMethod]
    public void TestDecrypt() {
        sut.SetKey(key);
        CustomAssertions.AssertDoesNotThrow(() => { sut.Decrypt(code); });
    }

    private DHUser<GaloisField> sut = new DHUser<GaloisField>(gfFactory, setup);

    private static GaloisFieldFactory gfFactory = new GaloisFieldFactory();
    private static DHSetup<GaloisField> setup = new DHSetup<GaloisField>(gfFactory);
    private static GaloisField key = new GaloisField(111);
    private static GaloisField message = new GaloisField(123);
    private static GaloisField code = new GaloisField(321);
}
