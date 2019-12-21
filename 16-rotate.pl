%16
%% app_dl1(A, B, B, A).

%% app_dl2(A - B, B, A).

%% app_dl3(A - B, B - C, A - C).

%% append_dl(L1, L2, R) :- app_dl3([L1 | X]-X, [L2 | Y]-Y, R-[]).

%% reverse1([], []).
%% reverse1([H|T],R) :- reverse1(T, L), append(L, [H], R).

%% reverse([], R, R).
%% reverse([H|T], Acc, R) :- reverse(T, [H|Acc], R).
%% reverse2(L, R) :- reverse(L, [], R).

%% rev_dl1([], L-L).
%% rev_dl1([X], [X|L]-L).
%% rev_dl1([H|T], L1-L3) :- rev_dl1(T, L1-L2), rev_dl1([H], L2-L3).
%% reverse3(L, R) :- rev_dl1(L, R-[]), !.

%% rev_dl2([], L-L).
%% rev_dl2([H, T], L1-L2) :- rev_dl2(T, L1-[H|L2]).
%% reverse4(L, R) :- rev_dl2(L, R-[]).

%% rev_dl3([], L-L).
%% rev_dl3([X], [X|L]-L).
%% rev_dl3([E1, E2|T], L1-L2) :- rev_dl3(T, L1-[E2, E1|L2]).
%% reverse5(L, R) :- rev_dl3(L, R-[]).

%% rotate([H|T], RL) :- append(T, [H], RL).
%% r_dl([H|T]-T1, R-S) :- app_dl3(T-T1, [H|L]-L, R-S).
rotate_dl([H|T] - [H|A], T-A).
