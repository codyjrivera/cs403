consult(helper3).
consult(problem3).
setof(R, setcover(3, [[a,b],[a,c],[a,d],[a,e],[c,e]], R), Answers). 	% Answers = [[[a,b],[a,d],[c,e]]]
setof(R, setcover(6, [[a,b],[a,c],[a,d],[a,e],[c,e]], R), Answers).     % Answers = All subsequences.
setof(R, setcover(0, [[a,b],[a,c],[a,d],[a,e],[c,e]], R), Answers).     % nothing
setof(R, setcover(1, [[a,a], [a, a], [a, a]], R), Answers).
setof(R, setcover(2, [[a,a], [a, a], [a, a]], R), Answers).



