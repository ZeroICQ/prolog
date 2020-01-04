person(h, a).
person(h, b).
person(h, c).
person(w, a).
person(w, b).
person(w, c).

precedes(person(h, a), person(h, b)).
precedes(person(h, a), person(h, c)).
precedes(person(h, a), person(w, a)).
precedes(person(h, a), person(w, b)).
precedes(person(h, a), person(w, c)).
precedes(person(h, b), person(h, c)).
precedes(person(h, b), person(w, a)).
precedes(person(h, b), person(w, b)).
precedes(person(h, b), person(w, c)).
precedes(person(h, c), person(w, a)).
precedes(person(h, c), person(w, b)).
precedes(person(h, c), person(w, c)).
precedes(person(w, a), person(w, b)).
precedes(person(w, a), person(w, c)).
precedes(person(w, b), person(w, c)).

equal(T, T).

member_(H, [H | _]).
member_(X, [_ | T]) :- member_(X, T).

select_(X, [X | T], T).
select_(X, [H | T], [H | T1]) :- select_(X, T, T1).

insert_(X, [], [X]).
insert_(X, [H | T], [X, H | T]) :- precedes(X, H).
insert_(X, [H | T], [H | T1]) :-
    precedes(H, X),
    insert_(X, T, T1).

have_another_wife(Couple, Persons) :-
    member(person(w, AnotherCouple), Persons),
    not(equal(Couple, AnotherCouple)).

illegal(Persons) :-
    member(person(w, Couple), Persons),
    length(Persons, Length),
    not(Length = 1),
    not(member(person(h, Couple), Persons)),
    not(have_another_wife(Couple, Persons)).

legal(state(left, _, Right)) :- not(illegal(Right)).
legal(state(right, Left, _)) :- not(illegal(Left)).

legal_persons(Persons) :- not(illegal(Persons)).

update_items([], _, Left, Right, Left, Right).
update_items([H|T], left, Left, Right, NewLeft, NewRight) :-
    select_(H, Right, R1),
    insert_(H, Left, L1),
    update_items(T, left, L1, R1, NewLeft, NewRight).
update_items([H|T], right, Left, Right, NewLeft, NewRight) :-
    select_(H, Left, L1),
    insert_(H, Right, R1),
    update_items(T, right, L1, R1, NewLeft, NewRight).

update_coast(left, right).
update_coast(right, left).

update(state(Coast, Left, Right), Persons, state(NewCoast, NewLeft, NewRight)) :-
    update_coast(Coast, NewCoast),
    update_items(Persons, NewCoast, Left, Right, NewLeft, NewRight).

move(state(left, Left, _), Persons) :-
    member_(Person1, Left),
    member_(Person2, Left),
    not(equal(Person1, Person2)),
    insert_(Person2, [Person1], Persons),
    legal_persons(Persons).
move(state(right, _, Right), Persons) :-
    member_(Person1, Right),
    member_(Person2, Right),
    not(equal(Person1, Person2)),
    insert_(Person2, [Person1], Persons),
    legal_persons(Persons).
move(state(left, Left, _), [Person]) :- member_(Person, Left).
move(state(right, _, Right), [Person]) :- member_(Person, Right).

final_state(state(right, [],
    [person(h, a),
     person(h, b),
     person(h, c),
     person(w, a),
     person(w, b),
     person(w, c)])).

initial_state(state, state(left,
    [person(h, a),
     person(h, b),
     person(h, c),
     person(w, a),
     person(w, b),
     person(w, c)], [])).

/*  Hill climbing framework for problem solving*/

/*
    solve_hill_climb(State,History,Moves) :-
    Moves is the sequence of moves to reach a desired final state
    from the current State, where History are the states
    visited previously.
*/

solve_hill_climb(State, _, []) :-
    final_state(State).

solve_hill_climb(State, History, [Move | Moves]) :-
    not(final_state(State)),
    hill_climb(State, Move),
    update(State, Move, State1),
    legal(State1),
    not(member(State1, History)),
    solve_hill_climb(State1, [State1 | History], Moves).

hill_climb(State, Move) :-
    findall(M, move(State, M), Moves),
    evaluate_and_order(Moves, State, [], MVs),
    member((Move, _), MVs).

value(state(_, _, Right), Value) :- length(Right, Value).

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

evaluate_and_order([], _, MVs, MVs).

insert(MV, [], [MV]).
insert((M, V), [(M1, V1) | MVs], [(M, V), (M1, V1) | MVs]) :- V >= V1.
insert((M, V), [(M1, V1) | MVs], [(M1, V1) | MVs1]) :- V < V1, insert((M, V), MVs, MVs1).

/*  Testing the Framework */
test_hill_climb(Problem, Moves)  :-
    initial_state(Problem, State),
    solve_hill_climb(State, [State], Moves).

solve_puzzle_hc(X) :- test_hill_climb(state, X).
