builtin(builtin(X)).

% Family 1: 
% Father: Bob

% father(father, son)
% father(bob, john).
% male(john).

%son(son, father)
%% son(X,Y) :- father(Y, X), male(X).
%% daughter(X,Y) :- father(Y,X), female(X).

%   Program 1.2: Biblical family relationships
% certanity threshold



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

clause_cf(son(X, Y), (father(Y, X), male(X)), 1).
clause_cf(gfather(X, Y), (father(Z, Y), father(X, Z)), 1).

clause_cf(male(john), true, 0.9).
clause_cf(father(bob, john), true, 0.9).
clause_cf(father(brad, bob), true, 0.95).
clause_cf(female(ann), true, 0.99).


clause_cf(male(andrew), true, 0.9).
clause_cf(father(rob, andrew), true, 0.9).
clause_cf(father(lee, rob), true, 0.2).

%% clause_cf(son(abracadabra, isaac), true, 0.26).

% solve(gfather(G, S), C, 0.1)