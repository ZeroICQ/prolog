:- use_module(library(clpfd)).


puzzle1([S, E, N, D], [M, O, R, E], [M, O, N, E ,Y]):-
       Vs = [S, E, N, D, M, O, R, Y],
       Vs ins 0..9, all_different(Vs),
       (S * 1000 + E * 100 + N * 10 + D) + (M * 1000 + O * 100 + R * 10 + E) #=
       (M * 10000 + O * 1000 + N * 100 + E * 10 + Y),
       S #\= 0, M #\= 0, label(Vs).

puzzle2([A, B, C], [B, C], [D, E, F, B, C]):-
       Vs = [A, B, C, D, E, F],
       Vs ins 0..9, all_different(Vs),
       (A * 100 + B * 10 + C) * (B * 10 + C) #=
       (D * 10000 + E * 1000 + F * 100 + B * 10 + C),
       A #\= 0, B #\= 0, D #\= 0, label(Vs).