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

%% precedes(w, [g, c]).
precedes(w, g).
precedes(w, c).
%% precedes(w, []).
precedes(g, c).
%% precedes(g, []).

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

%solve_puzzle(X):-test_dfs(wgc, X).



/*  Hill climbing framework for problem solving*/

/*  
    solve_hill_climb(State,History,Moves) :-
    Moves is the sequence of moves to reach a desired final state 
    from the current State, where History are the states 
    visited previously. 
*/

solve_hill_climb(State, History, []) :- 
    final_state(State).

solve_hill_climb(State, History, [Move | Moves]) :-
    hill_climb(State, Move),
    update(State, Move, State1),
    legal(State1),
    not(member(State1, History)),
    solve_hill_climb(State1, [State1 | History], Moves).

hill_climb(State, Move) :-
    findall(M, move(State, M), Moves),
    evaluate_and_order(Moves, State, [], MVs),
    member((Move, Value), MVs).

value(wgc(_, _, R), Value) :- length(R, Value).
    

/*   
    evaluate_and_order(Moves, State, SoFar, OrderedMVs) :-
    All the Moves from the current State are evaluated and 
    ordered as OrderedMVs. SoFar is an accumulator for 
    partial computations.
*/
evaluate_and_order([Move | Moves], State, MVs, OrderedMVs) :-
    update(State, Move, State1),
    value(State1, Value),
    insert((Move, Value), MVs, MVs1),
    evaluate_and_order(Moves, State, MVs1, OrderedMVs).

evaluate_and_order([], State, MVs, MVs).

insert(MV, [], [MV]).

insert((M, V), [(M1, V1) | MVs], [(M, V), (M1, V1) | MVs]) :- 
    V >= V1.

insert((M, V), [(M1, V1) | MVs], [(M1, V1) | MVs1]) :-
    V < V1, 
    insert((M, V), MVs, MVs1).

/*  Testing the Framework */
test_hill_climb(wgc, Moves)  :-
    initial_state(wgc, State),
    solve_hill_climb(State, [State], Moves).

solve_puzzle(X) :- 
    test_hill_climb(wgc, X).
