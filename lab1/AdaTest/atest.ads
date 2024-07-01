package ATest is
    procedure Assert_True(Condition : Boolean; Error_Message : String);
    procedure Assert_False(Condition : Boolean; Error_Message : String);

    generic
        type T is private;
    procedure Assert_Eq(Left, Right : T; Error_Message : String);

    generic
        type T is private;
    procedure Assert_Neq(Left, Right : T; Error_Message : String);

    procedure Print_Summary;
end ATest;
