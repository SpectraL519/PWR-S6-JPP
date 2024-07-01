extGcd(A, 0, 1, 0, A).
extGcd(A, B, X, Y, Z) :-
    B > 0,
    Tmp is A mod B,
    extGcd(B, Tmp, X1, Y1, Z),
    X is Y1,
    Y is X1 - Y1 * (A // B).

de(A, B, X, Y, Z) :-
    extGcd(A, B, X, Y, Z).


main :-
    A = 30,
    B = 42,
    de(A, B, X, Y, Z),
    format("Solution to the equation ~dx + ~dy = gcd(~d, ~d):~n", [A, B, A, B]),
    format("x = ~d | y = ~d | gcd(~d, ~d) = ~d~n", [X, Y, A, B, Z]),
    format("Check: ~d * ~d + ~d * ~d = ~d~n", [A, X, B, Y, A * X + B * Y]).

:- main.
