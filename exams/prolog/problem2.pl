
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).
shared_grandparent(G, X, Y) :- grandparent(G, X), grandparent(G, Y).    
sibling(X, Y) :- parent(A, X), parent(A, Y), X \= Y.

% bugfix siblings can not be double cousins now
double_cousin(X, Y) :- setof(G, (shared_grandparent(G, X, Y), X \= Y, \+(sibling(X, Y))), S),
                       length(S, 4).
