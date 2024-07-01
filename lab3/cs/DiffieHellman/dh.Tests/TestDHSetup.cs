namespace dh.Tests;

[TestClass]
public class TestDHSetup {
    [DataTestMethod]
    [DataRow(128, 0UL, 1)]
    [DataRow(2, 3UL, 8)]
    [DataRow(0, 5UL, 0)]
    [DataRow(GaloisField.ORDER - 1, 2UL, 1)]
    public void TestPower(long baseValue, ulong exponent, long expectedValue) {
        GaloisField gfBase = new GaloisField(baseValue);
        GaloisField expected = new GaloisField(expectedValue);

        Assert.AreEqual(sut.Power(gfBase, exponent), expected);
    }

    private static DHSetup<GaloisField> sut = new DHSetup<GaloisField>(new GaloisFieldFactory());
}
