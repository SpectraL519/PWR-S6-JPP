package body Algebra is

    function Factorial(N : Unsigned_64) return Unsigned_64 is
        Factorial : Unsigned_64 := 1;
    begin
        if N <= 1 then
            return Factorial;
        else
            for I in 2 .. N loop
                Factorial := Factorial * I;
            end loop;
            return Factorial;
        end if;
    end Factorial;

    function GCD(A, B : Unsigned_64) return Unsigned_64 is
        Acpy : Unsigned_64 := A;
        Bcpy : Unsigned_64 := B;
        Tmp : Unsigned_64;
    begin
        while Bcpy > 0 loop
            Tmp := Bcpy;
            Bcpy := Acpy mod Bcpy;
            Acpy := Tmp;
        end loop;

        return Unsigned_64(Acpy);
    end GCD;

    function Extended_GCD(A, B : Long_Integer; X, Y : out Long_Integer) return Long_Integer is
        Acpy : Long_Integer := A;
        Bcpy : Long_Integer := B;
        X1 : Long_Integer := 0;
        Y1 : Long_Integer := 1;
        X2 : Long_Integer := 1;
        Y2 : Long_Integer := 0;
    begin
        while Bcpy /= 0 loop
            declare
                Quotient : Long_Integer := Acpy / Bcpy;
                Remainder : Long_Integer := Acpy mod Bcpy;
                X_Tmp : Long_Integer;
                Y_Tmp : Long_Integer;
            begin
                X_Tmp := X2 - Quotient * X1;
                Y_Tmp := Y2 - Quotient * Y1;
                Acpy := Bcpy;
                Bcpy := Remainder;
                X2 := X1;
                Y2 := Y1;
                X1 := X_Tmp;
                Y1 := Y_Tmp;
            end;
        end loop;

        X := X2;
        Y := Y2;
        return Acpy;
    end Extended_GCD;

    function Solve_Diophantine(A, B, C : Long_Integer) return Diophantine_Solution is
        X, Y : Long_Integer;
        G : Long_Integer := Extended_GCD(A, B, X, Y);
    begin
        if C mod G /= 0 then
            return (0, 0);
        else
            return (X * (C / G), Y * (C / G));
        end if;
    end Solve_Diophantine;


    function R_Factorial(N : Unsigned_64) return Unsigned_64 is
    begin
        if N <= 1 then
            return 1;
        end if;

        return N * R_Factorial(N - 1);
    end R_Factorial;

    function R_GCD(A, B : Unsigned_64) return Unsigned_64 is
    begin
        if B = 0 then
            return A;
        end if;

        return R_GCD(B, A mod B);
    end R_GCD;

    function R_Extended_GCD(A, B : Long_Integer; X, Y : out Long_Integer) return Long_Integer is
        X1, Y1, G : Long_Integer;
    begin
        if B = 0 then
            X := 1;
            Y := 0;
            return A;
        end if;

        G := R_Extended_GCD(B, A mod B, X1, Y1);
        X := Y1;
        Y := X1 - (A / B) * Y1;
        return G;
    end R_Extended_GCD;

    function R_Solve_Diophantine(A, B, C : Long_Integer) return Diophantine_Solution is
        X, Y : Long_Integer;
        G : Long_Integer := R_Extended_GCD(A, B, X, Y);
    begin
        if C mod G /= 0 then
            return (0, 0);
        else
            return (X * (C / G), Y * (C / G));
        end if;
    end R_Solve_Diophantine;

end Algebra;
