consult(problem6).
setof([X,Y], (bools([X,Y]), solve(and(or(X,Y),not(X)), true)), Answers).	% Answers = [[false,true]]
setof([X,Y,Z], (bools([X,Y,Z]), solve(or(not(and(X,Y)),Z), false)), Answers).	% Answers = [[true,true,false]]
%implication
setof([X, Y], (bools([X, Y]), solve(not(and(X, not(Y))), true)), Answers). 
