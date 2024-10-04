{-# OPTIONS_FRONTEND -Wno-missing-signatures #-}

module Ackermann where

data Term = S Term | O
 deriving (Eq,Show)

ackermann O n = S n
ackermann (S m) O = ackermann m (S O)
ackermann (S m) (S n) = ackermann m (ackermann (S m) n)

peano2int O = 0
peano2int (S n) = peano2int n + 1

ack_2_2 = ackermann (S (S O)) (S (S O))

ack_3_2 = ackermann (S (S (S O))) (S (S O))

ack_3_3 = ackermann (S (S (S O))) (S (S (S O)))

ack_3_4 = ackermann (S (S (S O))) (S (S (S (S O))))

ack_3_5 = ackermann (S (S (S O))) (S (S (S (S (S O)))))

ack_3_6 = ackermann (S (S (S O))) (S (S (S (S (S (S O))))))

ack_3_7 = ackermann (S (S (S O))) (S (S (S (S (S (S (S O)))))))

ack_3_9 = ackermann (S (S (S O))) (S (S (S (S (S (S (S (S (S O)))))))))

ack_3_10 = ackermann (S (S (S O))) (S (S (S (S (S (S (S (S (S (S O))))))))))

ack_4_0 = ackermann (S (S (S (S O)))) O

ack_4_1 = ackermann (S (S (S (S O)))) (S O)

main = peano2int ack_3_9
