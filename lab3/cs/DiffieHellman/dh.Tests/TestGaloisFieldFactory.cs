namespace dh.Tests;

[TestClass]
public class TestGaloisFieldFactory {
    [TestMethod]
    public void TestFromLong() {
        Assert.AreEqual(sut.FromLong(gfValue), new GaloisField(gfValue));
    }

    [TestMethod]
    public void TestClone() {
        GaloisField gf = new GaloisField(gfValue);
        Assert.AreEqual(sut.Clone(gf), gf);
    }

    private static GaloisFieldFactory sut = new GaloisFieldFactory();
    private const long gfValue = 519;
}
