with Algebra;
with ATest;

with Interfaces; use Interfaces;

procedure Test_Algebra is

    procedure Test_Factorial is
        procedure Assert_Eq is new ATest.Assert_Eq(Unsigned_64);

        function Error_Message(Expected, Argument : in Unsigned_64; Recursive: Boolean) return String is
            Func : String := (if Recursive then "R_Factorial" else "Factorial");
        begin
            return "Expected ` " & Func & " ` result: " & Expected'Image & " for argument: " & Argument'Image;
        end Error_Message;

        N : constant array (1 .. 5) of Unsigned_64 := (0, 1, 2, 3, 4);
        F : constant array (1 .. 5) of Unsigned_64 := (1, 1, 2, 6, 24);
    begin
        for I in 1 .. 5 loop
            Assert_Eq(Algebra.Factorial(N(I)), F(I), Error_Message(F(I), N(I), False));
            Assert_Eq(Algebra.R_Factorial(N(I)), F(I), Error_Message(F(I), N(I), True));
        end loop;
    end Test_Factorial;

    procedure Test_GCD is
        procedure Assert_Eq is new ATest.Assert_Eq(Unsigned_64);

        function Error_Message(A, B, GCD : in Unsigned_64; Recursive: Boolean) return String is
            Func : String := (if Recursive then "R_GCD" else "GCD");
        begin
            return "Expected `" & Func & "` result: " & GCD'Image & " for arguments: (" &
                   A'Image & "," & B'Image & ")";
        end Error_Message;

        A   : constant array (1 .. 5) of Unsigned_64 := (0, 5, 7, 9, 12);
        B   : constant array (1 .. 5) of Unsigned_64 := (5, 0, 5, 6, 18);
        GCD : constant array (1 .. 5) of Unsigned_64 := (5, 5, 1, 3, 6);
    begin
        for I in 1 .. 5 loop
            Assert_Eq(Algebra.GCD(A(I), B(I)), GCD(I), Error_Message(A(I), B(I), GCD(I), False));
            Assert_Eq(Algebra.R_GCD(A(I), B(I)), GCD(I), Error_Message(A(I), B(I), GCD(I), True));
        end loop;
    end Test_GCD;

    procedure Test_Solve_Diophantine is
        function Error_Message(A, B, C : in Long_Integer; Recursive: Boolean) return String is
            Func : String := (if Recursive then "R_Solve_Diophantine" else "Solve_Diophantine");
        begin
            return "`" & Func & "` result does not satisfy the exuation: " &
                   A'Image & "x + " & B'Image & "y = " & C'Image;
        end Error_Message;

        function Check_Solution(A, B, C: Long_Integer; Solution : Algebra.Diophantine_Solution) return Boolean is
        begin
            return A * Solution.x + B * Solution.y = C;
        end Check_Solution;

        A : constant array (1 .. 3) of Long_Integer := (57, 14, 25);
        B : constant array (1 .. 3) of Long_Integer := (15, 21, 10);
        C : constant array (1 .. 3) of Long_Integer := (3, 7, 5);
        Solution : Algebra.Diophantine_Solution;
        R_Solution : Algebra.Diophantine_Solution;
    begin
        for I in 1 .. 3 loop
            Solution := Algebra.Solve_Diophantine(A(I), B(I), C(I));
            ATest.Assert_True(
                Check_Solution(A(I), B(I), C(I), Solution), Error_Message(A(I), B(I), C(I), False));

            R_Solution := Algebra.R_Solve_Diophantine(A(I), B(I), C(I));
            ATest.Assert_True(
                Check_Solution(A(I), B(I), C(I), R_Solution), Error_Message(A(I), B(I), C(I), True));
        end loop;
    end Test_Solve_Diophantine;

begin
    Test_Factorial;
    Test_GCD;
    Test_Solve_Diophantine;
    ATest.Print_Summary;
end Test_Algebra;
