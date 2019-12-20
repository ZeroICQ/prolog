%13
member_(H, [H | _]).
member_(X, [_ | T]) :- member_(X, T).

select_(X, [X | T], T).
select_(X, [H | T], [H | T1]) :- select_(X, T, T1).

solve_dfs(S, _, []) :- final_state(S).
solve_dfs(S, H, [M | Ms]) :-
    move(S, M),
    update(S, M, S1),
    legal(S1),
    not(member_(S1, H)),
    solve_dfs(S1, [S1 | H], Ms).

test_dfs(Name, Ms) :-
    initial_state(Name, S),
    solve_dfs(S, [S], Ms).

initial_state(wgc, wgc(l, [w, g, c], [])).

move(wgc(l, L, _), M) :- member_(M, L).
move(wgc(r, _, R), M) :- member_(M, R).
move(wgc(_, _, _), alone).

update(wgc(C, L, R), M, wgc(C1, L1, R1)) :-
    update_coast(C, C1),
    update_items(M, C1, L, R, L1, R1).

update_coast(l, r).
update_coast(r, l).

update_items(alone, _, L, R, L, R).
update_items(M, l, L, R, L1, R1) :-
    select_(M, R, R1),
    insert_(M, L, L1).
update_items(M, r, L, R, L1, R1) :-
    select_(M, L, L1),
    insert_(M, R, R1).

%precedes(w, _).
%precedes(_, c).
%precedes(w, [g, c]).
precedes(w, g).
%precedes(w, c).
%precedes(w, []).
%precedes(g, c).
%precedes(g, []).

insert_(X, [], [X]).
insert_(X, [H | T], [X, H | T]) :- precedes(X, H).
insert_(X, [H | T], [H | T1]) :-
    precedes(H, X),
    insert_(X, T, T1).

legal(wgc(l, _, R)) :- not(illegal(R)).
legal(wgc(r, L, _)) :- not(illegal(L)).

illegal(C) :- member_(w, C), member_(g, C).
illegal(C) :- member_(g, C), member_(c, C).

final_state(wgc(r, [], [w, g, c])).

solve_puzzle(X):-test_dfs(wgc, X).
