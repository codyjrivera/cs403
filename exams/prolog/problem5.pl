
flipFirst([], []).
flipFirst([L | Ls], [[L] | Rs]) :- flipFirst(Ls, Rs).

flipAdd([], [], []) :- !.
flipAdd([], _, []) :- !.
flipAdd(_, [], []) :- !.
flipAdd([L | Ls], [I | Is], [[L | I] | Rs]) :- flipAdd(Ls, Is, Rs).

flip([L], R) :- flipFirst(L, R).
flip([L | Ls], R) :- flip(Ls, Rp), flipAdd(L, Rp, R).

mapall(_, [], []).
mapall(F, [L | Ls], [R | Rs]) :- call(F, L, R), mapall(F, Ls, Rs).

multimap(F, [], []).
multimap(F, L, R) :- flip(L, Lp), mapall(F, Lp, R).

