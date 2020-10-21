consult(problem7).
setof(N, dlength([a,b,c,d|T]-T, N), Answers).			% Answers = [4]
setof(N, dlength(A-A, N), Answers).
setof(X, dreverse([a,b,c,d|T]-T, X-Y), Answers).		% Answers = [[d,c,b,a|Y]]
setof(X, dreverse(A-A, X-Y), Answers).
setof(X, drotateleft([a,b,c,d,e,f|T]-T, 2, X-Y), Answers).	% Answers = [[c,d,e,f,a,b|Y]]
setof(X, drotateright([a,b,c,d,e,f|T]-T, 2, X-Y), Answers).	% Answers = [[e,f,a,b,c,d|Y]]
