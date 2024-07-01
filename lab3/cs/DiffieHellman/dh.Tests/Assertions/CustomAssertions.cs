namespace dh.Tests.Assertions;

public class CustomAssertions {
    public static void AssertDoesNotThrow(Action action) {
        try {
            action();
        }
        catch (Exception exception) {
            Assert.Fail("Expected no exception, but got: " + exception.Message);
        }
    }
}
