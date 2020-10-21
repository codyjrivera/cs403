consult(family1).
consult(problem1).
setof(X, half_cousin(X,sadie), Answers).	% Answers = [steven]
setof(Y, half_cousin(steven,Y), Answers).	% Answers = [brayden, sadie]
setof(X, half_cousin(brayden, X), Answers). % Answers = [steven]
