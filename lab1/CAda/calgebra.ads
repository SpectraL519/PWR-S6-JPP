with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;

package CAlgebra is
    type Diophantine_Solution is record
        X : Long_Integer;
        Y : Long_Integer;
    end record;

    pragma Convention(C, Diophantine_Solution);


    function Factorial(N : Unsigned_64) return Unsigned_64;
    function GCD(A, B : Unsigned_64) return Unsigned_64;
    function Solve_Diophantine(A, B, C : Long_Integer) return Diophantine_Solution;

    pragma Import(C, Factorial, "factorial");
    pragma Import(C, GCD, "gcd");
    pragma Import(C, Solve_Diophantine, "solve_diophantine");

end CAlgebra;
