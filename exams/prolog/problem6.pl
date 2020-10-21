
bools([]).
bools([true | Xs]) :- bools(Xs).
bools([false | Xs]) :- bools(Xs).

% simple sat solver
solve(true, true).
solve(false, false).

solve(not(A), false) :- solve(A, true).
solve(not(A), true) :- solve(A, false).

solve(or(A, B), C) :- solve(A, false), solve(B, C).
solve(or(A, _), true) :- solve(A, true).

solve(and(A, _), false) :- solve(A, false).
solve(and(A, B), C) :- solve(A, true), solve(B, C).

