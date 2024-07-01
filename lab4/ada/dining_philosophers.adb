with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Containers.Vectors; use Ada.Containers;

procedure Dining_Philosophers is
    -- Constants
    Num_Philosophers : constant := 5;
    Max_Meals : constant := 10;

    Max_Rand_Time : constant := 1000.0;
    Eating_Time_Offset : constant := 200;
    Thinking_Time_Offset : constant := 500;

    -- Cutlery utility
    protected type Cutlery is
        entry Grab;
        procedure Put_Down;
    private
        Locked : Boolean := False;
    end Cutlery;

    protected body Cutlery is
        entry Grab when not Locked is
        begin
            Locked := True;
        end Grab;

        procedure Put_Down is
        begin
            Locked := False;
        end Put_Down;
    end Cutlery;

    type Cutlery_Ptr is access all Cutlery;

    Cutlery_List : array (1..Num_Philosophers) of aliased Cutlery;

    -- Philosopher utility
    type Philosopher_Id is new Natural range 1 .. Num_Philosophers;

    type Philosopher_Record is record
        ID : Philosopher_Id;
        Meals_Eaten : Natural;
    end record;

    package Philosophers_Vectors is new Vectors(
        Index_Type => Natural,
        Element_Type => Philosopher_Record);

    subtype Philosopher_Vector is Philosophers_Vectors.Vector;

    function Left_Cutlery_Id(Pid : Philosopher_Id) return Natural is
    begin
        return Natural(Pid);
    end Left_Cutlery_Id;

    function Right_Cutlery_Id(Pid : Philosopher_Id) return Natural is
    begin
        if Pid = Num_Philosophers then
            return 1;
        end if;
        return Natural(Pid + 1);
    end Right_Cutlery_Id;

    function To_String(P : in Philosopher_Record) return String is
    begin
        return "(C:" & Natural'Image(Left_Cutlery_Id(P.ID))
             & " | P:" & Philosopher_Id'Image(P.ID)
             & " | C:" & Natural'Image(Right_Cutlery_Id(P.ID))
             & " | M:" & Natural'Image(P.Meals_Eaten) & ")";
    end To_String;

    protected Dining_list is
        entry Add(P_Id : Philosopher_Id; Meals_Eaten : Natural);
        entry Remove(P_Id : Philosopher_Id);

    private
        procedure Print;

        Philosophers : Philosopher_Vector;
        Locked : Boolean := False;
    end Dining_List;

    protected body Dining_list is
        entry Add(P_Id : Philosopher_Id; Meals_Eaten : Natural) when not Locked is
        begin
            Locked := True;
            Philosophers.Append(Philosopher_Record'(ID => P_Id, Meals_Eaten => Meals_Eaten));
            Print;
            Locked := False;
        end Add;

        entry Remove(P_Id : Philosopher_Id) when not Locked is
            Index_To_Remove : Natural;
        begin
            Locked := True;
            for I in Philosophers_Vectors.First_Index(Philosophers) .. Philosophers_Vectors.Last_Index(Philosophers) loop
                if Philosophers(I).ID = P_Id then
                    Index_To_Remove := I;
                    exit;
                end if;
            end loop;

            if Index_To_Remove /= Philosophers_Vectors.No_Index then
                Philosophers.Delete(Index_To_Remove);
            end if;

            Print;
            Locked := False;
        end Remove;

        procedure Print is
        begin
            Put("> ");
            for I in Philosophers_Vectors.First_Index(Philosophers) .. Philosophers_Vectors.Last_Index(Philosophers) loop
                Put(To_String(Philosophers(I)) & " ");
            end loop;
            New_Line;
        end Print;
    end Dining_list;

    function Random_Sleep_Time return Natural is
        Gen : Generator;
    begin
        Reset(Gen);
        return Natural(Float(Random(Gen)) * Max_Rand_Time);
    end Random_Sleep_Time;

    function To_Seconds(Milliseconds : Integer) return Duration is
        Seconds : constant Integer := Milliseconds / 1000;
    begin
        return Duration(Seconds);
    end To_Seconds;

    task type Philosopher_Thread is
        entry Start(Pid : Philosopher_Id; LC, RC : Cutlery_Ptr);
    end Philosopher_Thread;

    task body Philosopher_Thread is
        ID : Philosopher_Id;
        Left_Cutlery, Right_Cutlery : Cutlery_Ptr;
        Gen : Generator;
    begin
        accept Start (Pid : Philosopher_Id; LC, RC : Cutlery_Ptr) do
            ID := Pid;
            Left_Cutlery := LC;
            Right_Cutlery := RC;
        end Start;

        Reset (Gen);
        for Meals_Eaten in 1..Max_Meals loop
            -- think
            delay To_Seconds(Random_Sleep_Time + Thinking_Time_Offset);

            -- acquire cutlery
            Left_Cutlery.Grab;
            Right_Cutlery.Grab;

            -- eat
            Dining_list.Add(ID, Meals_Eaten);
            delay To_Seconds(Random_Sleep_Time + Eating_Time_Offset);

            -- release cutlery
            Dining_list.Remove(ID);
            Left_Cutlery.Put_Down;
            Right_Cutlery.Put_Down;
        end loop;
        Put_Line("[Finished eating:" & Philosopher_Id'Image(ID) & "]");
    end Philosopher_Thread;

    Philosophers : array (1 .. Num_Philosophers) of Philosopher_Thread;

begin
    Put_Line("Parameters:");
    Put_Line("    Num_Philosophers:" & Num_Philosophers'Image);
    Put_Line("    Max_Meals:" & Max_Meals'Image);
    Put_Line("Starting execution!"); New_Line;

    for I in Philosophers'Range loop
        declare
            Pid : Philosopher_Id := Philosopher_Id(I);
        begin
            Philosophers(I).Start(
                Philosopher_Id(I),
                Cutlery_List(Left_Cutlery_Id(Pid))'Access,
                Cutlery_List(Right_Cutlery_Id(Pid))'Access);
        end;
    end loop;
end Dining_Philosophers;
