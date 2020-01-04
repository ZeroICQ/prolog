dutch_reverse(L, RWB-[]) :-
    distr_dl_re(L, RWB-WB, WB-B, B-[]).

distr_dl_re([r(H)|T], R-R1, W, B) :-distr_dl_re(T, R-[r(H)|R1], W, B).
distr_dl_re([w(H)|T], R, W-W1, B) :-distr_dl_re(T, R, W-[w(H)|W1], B).
distr_dl_re([b(H)|T], R, W, B-B1) :-distr_dl_re(T, R, W, B-[b(H)|B1]).
distr_dl_re([], R-R, W-W, B-B).
