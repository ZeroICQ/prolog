:- use_module(library(clpfd)).

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

/* my code */
/*(Missionaries, cannibals)*/
missionaries(bank(M, _), M).
cannibals(bank(_, C), C).
missionaries(move(M, C), M).
cannibals(move(M, C), C).

initial_state(mac, mac(l, bank(3, 3), bank(0, 0))).
final_state(mac(r, bank(0, 0), bank(3, 3))).
move(mac(l, L, R), M) :- find_move(L, R, M).
move(mac(r, L, R), M) :- find_move(R, L, M).

value(mac(_, _, bank(M, C)), V) :- V is M+C.

update(mac(D, L, R), M, mac(D1, L1, R1)) :-
    update_direction(D, D1),
    update_people(M, D, L, R, L1, R1).


update_people(M, l, L, R, L1, R1) :-
    update_people_(M, L, R, L1, R1).

update_people(M, r, L, R, L1, R1) :-
    update_people_(M, R, L, R1, L1).

update_people_(move(M, C), bank(FM, FC), bank(TM, TC), bank(FM1 , FC1), bank(TM1, TC1)) :-
    FM1 is FM-M,
    FC1 is FC-C,
    TM1 is TM+M,
    TC1 is TC+C.


update_direction(l, r).
update_direction(r, l).

find_move(From, To, move(1, 0)).
find_move(From, To, move(1, 1)).
find_move(From, To, move(0, 1)).
find_move(From, To, move(2, 0)).
find_move(From, To, move(0, 2)).

legal(mac(_, L, R)) :-
    legal_bank(L),
    legal_bank(R).

legal_bank(bank(M, C)) :-
    M >= 0,
    M =< 3,
    C >= 0,
    C =< 3,
    ((M >= C, M > 0); M=0).

/*  Testing the Framework */
test_hill_climb(Problem, Moves)  :-
    initial_state(Problem, State),
    solve_hill_climb(State, [State], Moves).

solve_puzzle(X):-test_hill_climb(mac, X).
