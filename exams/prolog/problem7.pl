
null(X-Y) :- unify_with_occurs_check(X,Y).

dlength(A-B, 0) :- null(A-B), !.
dlength([_|As]-B, N) :- dlength(As-B, Np), N is Np + 1.

dapp(A-B, B-C, A-C).

dreverse(A-B, A-B) :- null(A-B), !.
dreverse([A|As]-B, C-D) :- dreverse(As-B, Cs-Ds), dapp(Cs-Ds, [A|Es]-Es, C-D).

drotateleft(A-B, _, A-B) :- null(A-B), !.
drotateleft(A-B, N, A-B) :- N =< 0, !.
drotateleft([A | As]-[A | B], N, C-D) :- Np is N - 1, drotateleft(As-B, Np, C-D).

dtakedrop(A-B, N, C-C, A-B) :- N =< 0, !.
dtakedrop([A|As]-B, N, [A|C]-D, E-F) :- Np is N - 1, dtakedrop(As-B, Np, C-D, E-F).

drotateright(A-B, _, A-B) :- null(A-B), !.
drotateright(A-B, N, A-B) :- N =< 0, !.
drotateright(A-B, N, C-D) :- Np is N - 1, 
                             dlength(A-B, L), Lp is L - 1,
                             dtakedrop(A-B, Lp, Ap-Bp, [Cp | Dp]-Dp),
                             drotateright([Cp | Ap]-Bp, Np, C-D).
