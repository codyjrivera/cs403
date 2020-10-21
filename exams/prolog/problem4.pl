
reject(_, [], []).
reject(P, [L | Ls], [L | Rs]) :- \+(call(P, L)), !, reject(P, Ls, Rs).
reject(P, [_ | Ls], Rs) :- reject(P, Ls, Rs).

