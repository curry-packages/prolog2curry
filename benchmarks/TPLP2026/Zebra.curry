{-# OPTIONS_FRONTEND -Wno-missing-signatures #-}

module Zebra where

data Term = Red
          | English
          | Spanish
          | Dog
          | Coffee
          | Ukrainian
          | Tea
          | Green
          | Ivory
          | Snails
          | Winstons
          | Yellow
          | Milk
          | Chesterfields
          | Fox
          | Kools
          | Horse
          | Orange_juice
          | Lucky_strikes
          | Japanese
          | Parliaments
          | Norwegian
          | Blue
          | Zebra
          | Water
          | House Term Term Term Term Term
 deriving (Eq,Show)

puzzle houses
  | (houses =:= all_houses)
     && (my_member (House Red English _ _ _) houses
          && (my_member (House _ Spanish Dog _ _) houses
               && (my_member (House Green _ _ Coffee _) houses
                    && (my_member (House _ Ukrainian _ Tea _) houses
                         && (right_of (House Green _ _ _ _)
                              (House Ivory _ _ _ _)
                              houses
                              && (my_member (House _ _ Snails _ Winstons)
                                   houses
                                   && (my_member (House Yellow _ _ _ Kools)
                                        houses
                                        && ((houses
                                              =:= [_
                                                  ,_
                                                  ,House _ _ _ Milk _
                                                  ,_
                                                  ,_])
                                             && ((houses
                                                   =:= (House _ Norwegian _ _
                                                         _
                                                         : _))
                                                  && (next_to
                                                       (House _ _ _ _
                                                         Chesterfields)
                                                       (House _ _ Fox _ _)
                                                       houses
                                                       && (next_to
                                                            (House _ _ _ _
                                                              Kools)
                                                            (House _ _ Horse _
                                                              _)
                                                            houses
                                                            && (my_member
                                                                 (House _ _ _
                                                                   Orange_juice
                                                                   Lucky_strikes)
                                                                 houses
                                                                 && (my_member
                                                                      (House _
                                                                        Japanese
                                                                        _
                                                                        _
                                                                        Parliaments)
                                                                      houses
                                                                      && (next_to
                                                                           (House
                                                                             _
                                                                             Norwegian
                                                                             _
                                                                             _
                                                                             _)
                                                                           (House
                                                                             Blue
                                                                             _
                                                                             _
                                                                             _
                                                                             _)
                                                                           houses
                                                                           && (my_member
                                                                                (House
                                                                                  _
                                                                                  _
                                                                                  Zebra
                                                                                  _
                                                                                  _)
                                                                                houses
                                                                                && my_member
                                                                                    (House
                                                                                      _
                                                                                      _
                                                                                      _
                                                                                      Water
                                                                                      _)
                                                                                    houses)))))))))))))))
  = True

all_houses =
  [House _ _ _ _ _
  ,House _ _ _ _ _
  ,House _ _ _ _ _
  ,House _ _ _ _ _
  ,House _ _ _ _ _]

right_of a b (b : (a : _)) = True
right_of a b (_ : y) | right_of a b y = True

next_to a b (a : (b : _)) = True
next_to a b (b : (a : _)) = True
next_to a b (_ : y) | next_to a b y = True

my_member x (x : _) = True
my_member x (_ : y) | my_member x y = True

main | puzzle houses = houses
  where
    houses free
