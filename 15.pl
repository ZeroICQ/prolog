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

/*  Testing the Framework */
test_hill_climb(Problem, Moves)  :-
    initial_state(Problem, State),
    solve_hill_climb(State, [State], Moves).

insert_(X, [], [X]).
insert_(X, [H | T], [X, H | T]) :- precedes(X, H).
insert_(X, [H | T], [H | T1]) :-
    precedes(H, X),
    insert_(X, T, T1).

precedes(h1, h2).
precedes(h1, h3).
precedes(h1, w1).
precedes(h1, w2).
precedes(h1, w3).

precedes(h2, h3).
precedes(h2, w1).
precedes(h2, w2).
precedes(h2, w3).

precedes(h3, w1).
precedes(h3, w2).
precedes(h3, w3).

precedes(w1, w2).
precedes(w1, w3).

precedes(w2, w3).


select_(X, [X | T], T).
select_(X, [H | T], [H | T1]) :- select_(X, T, T1).

% select element, return tail
select_person(X, [X | T], T).
select_person(X, [H | T], T1) :- select_person(X, T, T1).



initial_state(wah, state(left, [h1, h2, h3, w1, w2, w3], [])).
final_state(state(right, [], [h1, h2, h3, w1, w2, w3])).

move(state(left, Left, Right), Move) :- move_(Left, Right, Move).
move(state(right, Left, Right), Move) :- move_(Right, Left, Move).

move_(From, To, [Person]) :- member(Person, From).

move_(From, To, [Person1, Person2]) :- 
    select_person(Person1, From, AfterFirstSelectFrom),
    select_person(Person2, AfterFirstSelectFrom, NewFrom),
    legal_move([Person1, Person2]).

legal_move([Person1, Person2]) :- not(illegal_pair(Person1, Person2)).

illegal_pair(h1, w2).
illegal_pair(h1, w3).

illegal_pair(h2, w1).
illegal_pair(h2, w3).

illegal_pair(h3, w1).
illegal_pair(h3, w2).

couple(h1, w1).
couple(h2, w2).
couple(h3, w3).

value(state(_, _, Right), Value) :- length(Right, Value).

update(state(Coast, Left, Right), Move, state(NewCoast, NewLeft, NewRight)) :-
    update_coast(Coast, NewCoast),
    update_items(Move, Coast, Left, Right, NewLeft, NewRight).

update_coast(left, right).
update_coast(right, left).

update_items(Move, left, Left, Right, NewLeft, NewRight) :- update_items_(Move, Left, Right, NewLeft, NewRight).
update_items(Move, right, Left, Right, NewLeft, NewRight) :- update_items_(Move, Right, Left, NewRight, NewLeft).


% todo: check len
update_items_([], From, To, From, To).
update_items_([H | T], From, To, NewFrom, NewTo) :- 
    select_(H, From, F1),
    insert_(H, To, T1),
    update_items_(T, F1, T1, NewFrom, NewTo).

legal(state(_, Left, Right)) :-
    legal_coast(Left),
    legal_coast(Right).


legal_coast(Coast) :-
    length(Coast, Length),
    Length < 2.

legal_coast(Coast) :-
    length(Coast, PersonsOnCoast),
    PersonsOnCoast >= 2,
    wives(Coast, Wives),
    length(Wives, WivesLength),
    (PersonsOnCoast = WivesLength; check_wife_has_husband(Wives, Coast)).

check_wife_has_husband([], Coast).
check_wife_has_husband([H | T], Coast) :-
    couple(Husband, H),
    member(Husband, Coast),
    check_wife_has_husband(T, Coast).


wives(Coast, Wives) :- wives_(Coast, [], Wives).


wives_([], In, In).
wives_([H | T], In, [H | B]) :-
    couple(_, H),
    wives_(T, In, B).

wives_([H | T], In, B) :-
    not(couple(_, H)),
    wives_(T, In, B).

% outer cycle
legal_([E]). % check len

legal_([H | T]) :-
    legal_inner_(H, T),
    legal_(T).

% inner cycle
legal_inner_(Person, []).

legal_inner_(Person, [H | T]) :-
    not(illegal_pair(Person, H)),
    legal_inner_(Person, T).

solve_puzzle(Moves) :- test_hill_climb(wah, Moves).
sp(X) :- solve_puzzle(X).
