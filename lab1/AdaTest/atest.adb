with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body ATest is

    Test_Count : Integer := 0;
    Success_Count : Integer := 0;
    Failure_Count : Integer := 0;

    procedure Assert_True(Condition : Boolean; Error_Message : String) is
    begin
        Test_Count := Test_Count + 1;
        if not Condition then
            Failure_Count := Failure_Count + 1;
            Put_Line("ERROR [Assert_True]: " & Error_Message);
            return;
        end if;
        Success_Count := Success_Count + 1;
    end Assert_True;

    procedure Assert_False(Condition : Boolean; Error_Message : String) is
    begin
        Test_Count := Test_Count + 1;
        if Condition then
            Failure_Count := Failure_Count + 1;
            Put_Line("ERROR [Assert_False]: " & Error_Message);
            return;
        end if;
        Success_Count := Success_Count + 1;
    end Assert_False;

    procedure Assert_Eq(Left, Right : T; Error_Message : String) is
    begin
        Test_Count := Test_Count + 1;
        if Left /= Right then
            Failure_Count := Failure_Count + 1;
            Put_Line("ERROR [Assert_Eq]: " & Error_Message);
            return;
        end if;
        Success_Count := Success_Count + 1;
    end Assert_Eq;

    procedure Assert_Neq(Left, Right : T; Error_Message : String) is
    begin
        Test_Count := Test_Count + 1;
        if Left = Right then
            Failure_Count := Failure_Count + 1;
            Put_Line("ERROR [Assert_Neq]: " & Error_Message);
            return;
        end if;
        Success_Count := Success_Count + 1;
    end Assert_Neq;

    procedure Print_Summary is
    begin
        Put_Line("ATest summary:");
        Put_Line("-------------------------------------");
        Put_Line("Total assertions: " & Test_Count'Image);
        Put_Line("Passed: " & Success_Count'Image);
        Put_Line("Failed: " & Failure_Count'Image);
    end Print_Summary;

end ATest;
