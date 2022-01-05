% Example: family relationships

% Structure of the family:
%
%                  Christine --- Antony  Maria --- Bill
%                    /    \              |
%                   /      \             |
%     Monica --- John       Alice --- Frank
%      /  \                   |
%     /    \                  |
%  Susan  Peter             Andrew


% husband relation
husband(christine,antony).
husband(maria    ,bill  ).
husband(monica   ,john  ).
husband(alice    ,frank ).

% child/mother relation
mother(john  ,christine ).
mother(alice ,christine ).
mother(frank ,maria     ).
mother(susan ,monica    ).
mother(peter ,monica    ).
mother(andrew,alice     ).

father(C,F) :- mother(C,M), husband(M,F).

grandfather(C,G) :- father(C,F), father(F,G).
grandfather(C,G) :- mother(C,M), father(M,G).

isHusband(Person) :- husband(_,Person).

goal1(Child) :- father(john,Child).
goal2(C,G)   :- grandfather(C,G).