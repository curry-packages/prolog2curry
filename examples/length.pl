% Sterlin/Shapiro: The Art of Prolog, program 3.17

% length of a list
length([],o).
length([X|Xs],s(N)) :- length(Xs,N).
