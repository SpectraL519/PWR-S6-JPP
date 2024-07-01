gcd(A, 0, A).
gcd(A, B, GCD) :-
    B > 0,
    C is A mod B,
    gcd(B, C, GCD).

coprime(X, Y) :-
    gcd(X, Y, GCD),
    GCD =:= 1.

totientHelper(_, 1, T, T) :- !.
totientHelper(N, M, Acc, T) :-
    M > 1,
    coprime(N, M),
    NewAcc is Acc + 1,
    NewM is M - 1,
    totientHelper(N, NewM, NewAcc, T).
totientHelper(N, M, Acc, T) :-
    M > 1,
    \+ coprime(N, M),
    NewM is M - 1,
    totientHelper(N, NewM, Acc, T).

totient(1, 1).
totient(N, T) :-
    N > 1,
    totientHelper(N, N, 1, T).


main :-
    N = 7919,
    totient(N, T),
    format("Euler's Totient function for N = ~d is ~d~n", [N, T]).

:- main.
