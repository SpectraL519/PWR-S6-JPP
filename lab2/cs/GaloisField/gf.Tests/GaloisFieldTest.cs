namespace gf.Tests;

[TestClass]
public class GaloisFieldTest {
    [TestMethod]
    public void ElementsShouldBeComparableByValue() {
        GaloisField a = new GaloisField(5);
        GaloisField b = new GaloisField(7);

        // Equals
        Assert.IsTrue(a.Equals(a));
        Assert.IsTrue(b.Equals(b));
        Assert.IsFalse(a.Equals(b));
        Assert.IsFalse(b.Equals(a));

        // operator ==
        Assert.AreEqual(a, a);
        Assert.AreEqual(b, b);

        // operator !=
        Assert.AreNotEqual(a, b);
        Assert.AreNotEqual(b, a);

        // operator <, operator <=
        Assert.IsTrue(a < b);
        Assert.IsTrue(a <= b);

        // operator >, operator >=
        Assert.IsTrue(b > a);
        Assert.IsTrue(b >= a);
    }

    [TestMethod]
    public void ElementsShouldBeInitializedModFieldOrder() {
        Assert.AreEqual(new GaloisField(), new GaloisField(GaloisField.ORDER));
    }

    [DataTestMethod]
    [DataRow(1, 3, 4)]
    [DataRow(0, GaloisField.ORDER, 0)]
    [DataRow(10, GaloisField.ORDER + 5, 15)]
    public void AdditionShouldBePerformedModFieldOrder(
        long lhsValue, long rhsValue, long expectedValue
    ) {
        GaloisField lhs = new GaloisField(lhsValue);
        GaloisField rhs = new GaloisField(rhsValue);
        GaloisField expected = new GaloisField(expectedValue);

        GaloisField result = lhs + rhs;
        Assert.AreEqual(result, expected);

        lhs += rhs;
        Assert.AreEqual(lhs, expected);
    }

    [DataTestMethod]
    [DataRow(10, 3, 7)]
    [DataRow(0, 5, 1234572)]
    [DataRow(10, GaloisField.ORDER + 5, 5)]
    public void SubtractionShouldBePerformedModFieldOrder(
        long lhsValue, long rhsValue, long expectedValue
    ) {
        GaloisField lhs = new GaloisField(lhsValue);
        GaloisField rhs = new GaloisField(rhsValue);
        GaloisField expected = new GaloisField(expectedValue);

        GaloisField result = lhs - rhs;
        Assert.AreEqual(result, expected);

        lhs -= rhs;
        Assert.AreEqual(lhs, expected);
    }

    [DataTestMethod]
    [DataRow(3, 4, 12)]
    [DataRow(5, 6, 30)]
    [DataRow(2, (GaloisField.ORDER + 3) / 2, 3)]
    public void MultiplicationShouldBePerformedModFieldOrder(
        long lhsValue, long rhsValue, long expectedValue
    ) {
        GaloisField lhs = new GaloisField(lhsValue);
        GaloisField rhs = new GaloisField(rhsValue);
        GaloisField expected = new GaloisField(expectedValue);

        GaloisField result = lhs * rhs;
        Assert.AreEqual(result, expected);

        lhs *= rhs;
        Assert.AreEqual(lhs, expected);
    }

    [DataTestMethod]
    [DataRow(12, 4, 3)]
    [DataRow(30, 6, 5)]
    public void DivisionShouldBePerformedModFieldOrder(
        long lhsValue, long rhsValue, long expectedValue
    ) {
        GaloisField lhs = new GaloisField(lhsValue);
        GaloisField rhs = new GaloisField(rhsValue);
        GaloisField expected = new GaloisField(expectedValue);

        GaloisField result = lhs / rhs;
        Assert.AreEqual(result, expected);

        lhs /= rhs;
        Assert.AreEqual(lhs, expected);
    }

    [TestMethod]
    public void DivisionByZeroShouldThrowAnException()
    {
        GaloisField lhs = new GaloisField(10);
        GaloisField rhs = new GaloisField(0);

        Assert.ThrowsException<DivideByZeroException>(() => { GaloisField result = lhs / rhs; });
        Assert.ThrowsException<DivideByZeroException>(() => { lhs /= rhs; });
    }

    public struct ArithmeticOperationData {
        public GaloisField lhs;
        public GaloisField rhs;
        public GaloisField expectedResult;
    }
}
