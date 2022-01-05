% Task: coloring a map

% we have four colors:
color(red).
color(yellow).
color(green).
color(blue).

% we have four countries which must have some color:
coloring(L1,L2,L3,L4) :- color(L1), color(L2), color(L3), color(L4).

% adjacent countries have different colors:
correctColoring(L1,L2,L3,L4) :-
        different(L1,L2),
        different(L1,L3),
        different(L1,L4),
        different(L2,L3),
        different(L2,L4),
        different(L3,L4).

% different(C1,C2) is true if the colors C1 and C2 are different.
different(red,yellow).
different(red,green).
different(red,blue).
different(yellow,red).
different(yellow,green).
different(yellow,blue).
different(green,red).
different(green,yellow).
different(green,blue).
different(blue,red).
different(blue,yellow).
different(blue,green).

mapColoring(L1,L2,L3,L4) :-
        coloring(L1,L2,L3,L4),
        correctColoring(L1,L2,L3,L4).


