
% nothing's gonna change my world
universe([], []).
universe([S | Ss], U) :- universe(Ss, Up), union(S, Up, U).

coverhelp(_, _, [], []).
coverhelp(N, [S | Ss], U, [S | Rs]) :- N > 0, Np is N - 1,
                                       diff(U, S, Up),
                                       coverhelp(Np, Ss, Up, Rs).
coverhelp(N, [_ | Ss], U, R) :- N > 0,
                                coverhelp(N, Ss, U, R).

setcover(N, L, R) :- universe(L, U), coverhelp(N, L, U, R).

