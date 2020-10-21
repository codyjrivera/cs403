consult(family2).
consult(problem2).
setof(X, double_cousin(X,kara), Answers).	% Answers = [george]
setof(Y, double_cousin(george,Y), Answers).	% Answers = [kara]
setof(X, double_cousin(zoe, X), Answers).   % Answers = []
setof(X, double_cousin(ralph, X), Answers). % doesn't crash
