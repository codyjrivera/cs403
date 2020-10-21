consult(problem4).
setof(R, reject(number, [a,5,b,3.14,c,89,[ ],2.7,d,[6]], R), Answers).		% Answers = [[a,b,c,[],d,[6]]]
assertz(sample(marker)).
setof(R, reject(sample, [x, y, z, marker, a, b, c, marker], R), Answers).
setof(R, reject(sample, [], R), Answers).
