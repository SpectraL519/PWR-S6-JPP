with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;

package Algebra is
    type Diophantine_Solution is record
        x : Long_Integer;
        y : Long_Integer;
    end record
        with Convention => C;


    function Factorial(N : Unsigned_64) return Unsigned_64
        with
            Export => True,
            Convention => C,
            External_Name => "Ada_Factorial";

    function GCD(A, B : Unsigned_64) return Unsigned_64
        with
            Export => True,
            Convention => C,
            External_Name => "Ada_GCD";

    function Solve_Diophantine(A, B, C : Long_Integer) return Diophantine_Solution
        with
            Export => True,
            Convention => C,
            External_Name => "Ada_Solve_Diophantine";


    function R_Factorial(N : Unsigned_64) return Unsigned_64
        with
            Export => True,
            Convention => C,
            External_Name => "Ada_R_Factorial";

    function R_GCD(A, B : Unsigned_64) return Unsigned_64
        with
            Export => True,
            Convention => C,
            External_Name => "Ada_R_GCD";

    function R_Solve_Diophantine(A, B, C : Long_Integer) return Diophantine_Solution
        with
            Export => True,
            Convention => C,
            External_Name => "Ada_R_Solve_Diophantine";
end Algebra;
