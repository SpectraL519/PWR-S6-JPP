with CAlgebra;
with ATest;

with Interfaces; use Interfaces;

procedure Test_CAlgebra is

    procedure Test_Factorial is
        procedure Assert_Eq is new ATest.Assert_Eq(Unsigned_64);

        function Error_Message(Expected, Argument : in Unsigned_64) return String is
        begin
            return "Expected `Factorial` result: " & Expected'Image & " for argument: " & Argument'Image;
        end Error_Message;

        N : constant array (1 .. 5) of Unsigned_64 := (0, 1, 2, 3, 4);
        F : constant array (1 .. 5) of Unsigned_64 := (1, 1, 2, 6, 24);
    begin
        for I in 1 .. 5 loop
            Assert_Eq(CAlgebra.Factorial(N(I)), F(I), Error_Message(F(I), N(I)));
        end loop;
    end Test_Factorial;

    procedure Test_GCD is
        procedure Assert_Eq is new ATest.Assert_Eq(Unsigned_64);

        function Error_Message(A, B, GCD : in Unsigned_64) return String is
        begin
            return "Expected `GCD` result: " & GCD'Image & " for arguments: (" &
                   A'Image & "," & B'Image & ")";
        end Error_Message;

        A   : constant array (1 .. 5) of Unsigned_64 := (0, 5, 7, 9, 12);
        B   : constant array (1 .. 5) of Unsigned_64 := (5, 0, 5, 6, 18);
        GCD : constant array (1 .. 5) of Unsigned_64 := (5, 5, 1, 3, 6);
    begin
        for I in 1 .. 5 loop
            Assert_Eq(CAlgebra.GCD(A(I), B(I)), GCD(I), Error_Message(A(I), B(I), GCD(I)));
        end loop;
    end Test_GCD;

    procedure Test_Solve_Diophantine is
        function Error_Message(A, B, C : in Long_Integer) return String is
        begin
            return "`Solve_Diophantine` result does not satisfy the exuation: " &
                   A'Image & "x + " & B'Image & "y = " & C'Image;
        end Error_Message;

        function Check_Solution(A, B, C: Long_Integer; Solution : CAlgebra.Diophantine_Solution) return Boolean is
        begin
            return A * Solution.x + B * Solution.y = C;
        end Check_Solution;

        A : constant array (1 .. 3) of Long_Integer := (57, 14, 25);
        B : constant array (1 .. 3) of Long_Integer := (15, 21, 10);
        C : constant array (1 .. 3) of Long_Integer := (3, 7, 5);
        Solution : CAlgebra.Diophantine_Solution;
        R_Solution : CAlgebra.Diophantine_Solution;
    begin
        for I in 1 .. 3 loop
            Solution := CAlgebra.Solve_Diophantine(A(I), B(I), C(I));
            ATest.Assert_True(
                Check_Solution(A(I), B(I), C(I), Solution), Error_Message(A(I), B(I), C(I)));
        end loop;
    end Test_Solve_Diophantine;

begin
    Test_Factorial;
    Test_GCD;
    Test_Solve_Diophantine;
    ATest.Print_Summary;
end Test_CAlgebra;
