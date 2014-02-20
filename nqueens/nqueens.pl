/*
  
  The N-Queens problem in Prolog
  Andrei de A. Formiga, 2014-02-15

*/

safe(X1, Y1, X2, Y2) :- Y1 =\= Y2, DX is abs(X1-X2), DY is abs(Y1-Y2), DX =\= DY.

allsafe(_ , _ , []).
allsafe(X1, Y1, [(X2, Y2) | T]) :- safe(X1, Y1, X2, Y2), allsafe(X1, Y1, T).

valid([]).
valid([(X, Y) | T]) :- allsafe(X, Y, T), valid(T).

range(N, N, [N]) :- N > 0.
range(N, M, [N | T]) :- N < M, N > 0, N1 is N+1, range(N1, M, T).

nqueens(N, L) :- nqueens_aux(N, 1, L).

nqueens_aux(N, N, [(N, C)]) :- range(1, N, R), member(C, R).
nqueens_aux(N, I, [(I, C) | S]) :- I < N, I1 is I+1, nqueens_aux(N, I1, S), 
                                   range(1, N, R), member(C, R), valid([(I, C) | S]).
