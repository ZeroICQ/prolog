builtin(builtin(X)).

father(abraham,isaac).
male(isaac).
father(haran,lot).
male(lot).

father(haran,milcah).
female(milcah).
father(haran,yiscah).
female(yiscah).

son(X,Y) :- father(Y,X), male(X).
daughter(X,Y) :- father(Y,X), female(X).

%   Program 1.2: Biblical family relationships




solve(true,1,T) :- !.
solve((A,B),C,T) :- !,
    solve(A,C1,T), 
    solve(B,C2,T), 
    minimum(C1,C2,C).

solve(A,1,T) :- builtin(A), !, A.
solve(A,C,T) :-
    clause_cf(A,B,C1), C1 > T, T1 is T/C1,
    solve(B,C2,T1), C is C1 * C2.

minimum(X,Y,X) :- X =< Y, !.
minimum(X,Y,Y) :- X > Y, !.

clause_cf(son(abraham, isaac), true, 0.8).
%% clause_cf(son(abracadabra, isaac), true, 0.26).

%solve(son(X, isaac), C, 0.9).