% Sterlin/Shapiro: The Art of Prolog, program 3.9

% the definition is inductively sequential so that it will be classified
% as a function
ackermann(o,N,s(N)).
ackermann(s(M),o,Val) :- ackermann(M,s(o),Val).
ackermann(s(M),s(N),Val) :- ackermann(s(M),N,Val1), ackermann(M,Val1,Val).

peano2int(o,0).
peano2int(s(N),J) :- peano2int(N,I), J is I+1.


ack_2_2(R) :- ackermann(s(s(o)),s(s(o)),R). % --> 7
ack_3_2(R) :- ackermann(s(s(s(o))),s(s(o)),R). % --> 29
ack_3_3(R) :- ackermann(s(s(s(o))),s(s(s(o))),R). % --> 61
ack_3_4(R) :- ackermann(s(s(s(o))),s(s(s(s(o)))),R). % --> 125
ack_3_5(R) :- ackermann(s(s(s(o))),s(s(s(s(s(o))))),R). % --> 253
ack_3_6(R) :- ackermann(s(s(s(o))),s(s(s(s(s(s(o)))))),R). % --> 509
ack_3_7(R) :- ackermann(s(s(s(o))),s(s(s(s(s(s(s(o))))))),R). % --> 1021
ack_3_9(R) :- ackermann(s(s(s(o))),s(s(s(s(s(s(s(s(s(o))))))))),R). % --> 4093
ack_3_10(R) :- ackermann(s(s(s(o))),s(s(s(s(s(s(s(s(s(s(o)))))))))),R). % --> 8189
ack_4_0(R) :- ackermann(s(s(s(s(o)))),o,R). % --> 13
ack_4_1(R) :- ackermann(s(s(s(s(o)))),s(o),R). % --> 65533


main(R) :- ack_3_9(A), peano2int(A,R).
