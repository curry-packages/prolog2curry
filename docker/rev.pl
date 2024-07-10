
app([],Xs,Xs).
app([X|Xs],Ys,[X|Zs]) :- app(Xs,Ys,Zs).

rev([],[]).
rev([X|Xs],R) :- rev(Xs,Zs), app(Zs,[X],R).
