
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).
shared_grandparent(G, X, Y) :- grandparent(G, X), grandparent(G, Y).

half_cousin(X, Y) :- setof(G, (shared_grandparent(G, X, Y), X \= Y), S),
                     length(S, 1).
