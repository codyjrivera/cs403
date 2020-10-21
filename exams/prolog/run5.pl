consult(helper5).
consult(problem5).
setof(R, multimap(sum, [[1,2,3,4],[5,6,7],[8,9,10,11],[12,13,14]], R), Answers).	% Answers = [[26,30,34]]
assertz(id(X, X)).
setof(R, multimap(prod, [[2,3,4],[5,6,7,8],[9,10,11]], R), Answers). 		% Answers = [[90,180,308]]
setof(R, multimap(id, [], R), Answers).
setof(R, multimap(id, [[], [1, 2, 3]], R), Answers).
setof(R, multimap(id, [[1, 2, 3], []], R), Answers).
