% Benchmark: naive reverse

add(o,N,N).
add(s(X),Y,s(Z)) :- add(X,Y,Z).

mult(o,_,o).
mult(s(X),Y,Z) :- mult(X,Y,XY), add(Y,XY,Z).

two(s(s(o))).
four(F) :- two(T), add(T,T,F).
nat16(N) :- four(F), mult(F,F,N).
nat256(N) :- nat16(M), mult(M,M,N).
nat4096(R) :- nat256(M), nat16(N), mult(M,N,R).

app([],Xs,Xs).
app([X|Xs],Ys,[X|Zs]) :- app(Xs,Ys,Zs).

rev([],[]).
rev([X|Xs],R) :- rev(Xs,Zs), app(Zs,[X],R).

natList(o,[]).
natList(s(X),[s(X)|Z]) :- natList(X,Z).

isList([]).
isList([_|Xs]) :- isList(Xs).

main :- nat4096(N), natList(N,L), rev(L,R), isList(R).

