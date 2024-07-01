namespace gf;

public class GaloisField {
    public GaloisField() {}

    public GaloisField(long value) {
        this.value = GaloisField.mod(value);
    }

    public GaloisField(GaloisField other) {
        this.value = other.value;
    }

    public static explicit operator long(GaloisField gf)
        => gf.value;

    public static implicit operator string(GaloisField gf)
        => gf.value.ToString();

    public bool Equals(GaloisField? other) {
        if (ReferenceEquals(other, null))
            return false;

        if (ReferenceEquals(this, other))
            return true;

        if (this.GetType() != other.GetType())
            return false;

        return this.value == other.value;
    }

    public override bool Equals(object? obj)
        => this.Equals(obj as GaloisField);

    public override int GetHashCode() => this.value.GetHashCode();

    public static bool operator ==(GaloisField lhs, GaloisField rhs) {
        if (ReferenceEquals(lhs, rhs))
            return true;

        if (ReferenceEquals(lhs, null) || ReferenceEquals(rhs, null))
            return false;

        return lhs.value == rhs.value;
    }

    public static bool operator !=(GaloisField lhs, GaloisField rhs)
        => !(lhs == rhs);

    public static bool operator <(GaloisField lhs, GaloisField rhs) {
        if (ReferenceEquals(lhs, null))
            return !ReferenceEquals(rhs, null);

        return lhs.value < rhs.value;
    }

    public static bool operator <=(GaloisField lhs, GaloisField rhs) {
        if (ReferenceEquals(lhs, null))
            return !ReferenceEquals(rhs, null);

        return lhs.value <= rhs.value;
    }

    public static bool operator >(GaloisField lhs, GaloisField rhs)
        => rhs < lhs;

    public static bool operator >=(GaloisField lhs, GaloisField rhs)
        => rhs <= lhs;

    public static GaloisField operator +(GaloisField lhs, GaloisField rhs)
        => new GaloisField(lhs.value + rhs.value);

    public static GaloisField operator -(GaloisField lhs, GaloisField rhs)
        => new GaloisField(GaloisField.ORDER + GaloisField.mod(lhs.value - rhs.value));

    public static GaloisField operator *(GaloisField lhs, GaloisField rhs)
        => new GaloisField(lhs.value * rhs.value);

    public static GaloisField operator /(GaloisField lhs, GaloisField rhs) {
        if (rhs.value == 0)
            throw new DivideByZeroException(string.Format("Cannot invert 0 mod({0})", GaloisField.ORDER));

        return new GaloisField(lhs.value * rhs.inverse());
    }

    public uint Order()
        => GaloisField.ORDER;

    private static long mod(long value)
        => value % GaloisField.ORDER;

    private long inverse() {
        long t = 0, newT = 1;
        long r = GaloisField.ORDER, newR = value;

        while (newR != 0) {
            long quotient = r / newR;
            long temp = t;
            t = newT;
            newT = temp - quotient * newT;

            temp = r;
            r = newR;
            newR = temp - quotient * newR;
        }

        return t < 0 ? (long)(t + GaloisField.ORDER) : (long)t;
    }

    public const uint ORDER = 1234577;
    private long value = 0;
}
