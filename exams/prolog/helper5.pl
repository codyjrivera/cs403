sum([ ],0).
sum([H|T], R) :- sum(T,X), R is H+X.

prod([ ],1).
prod([H|T], R) :- prod(T,X), R is H*X.

