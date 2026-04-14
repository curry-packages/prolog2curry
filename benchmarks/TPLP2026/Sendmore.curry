{-# OPTIONS_FRONTEND -Wno-missing-signatures #-}

module Sendmore where

smm
  | digit d
     && (digit e
          && ((d /= e)
               && (sumdigit 0 d e y c1
                    && (digit n
                         && ((n /= y)
                              && ((n /= e)
                                   && ((n /= d)
                                        && (digit r
                                             && ((r /= n)
                                                  && ((r /= y)
                                                       && ((r /= e)
                                                            && ((r /= d)
                                                                 && (sumdigit
                                                                      c1
                                                                      n
                                                                      r
                                                                      e
                                                                      c2
                                                                      && (digit
                                                                           o
                                                                           && ((o
                                                                                 /= r)
                                                                                && ((o
                                                                                      /= n)
                                                                                     && ((o
                                                                                           /= y)
                                                                                          && ((o
                                                                                                /= e)
                                                                                               && ((o
                                                                                                     /= d)
                                                                                                    && (sumdigit
                                                                                                         c2
                                                                                                         e
                                                                                                         o
                                                                                                         n
                                                                                                         c3
                                                                                                         && (leftdigit
                                                                                                              s
                                                                                                              && ((s
                                                                                                                    /= o)
                                                                                                                   && ((s
                                                                                                                         /= r)
                                                                                                                        && ((s
                                                                                                                              /= n)
                                                                                                                             && ((s
                                                                                                                                   /= y)
                                                                                                                                  && ((s
                                                                                                                                        /= e)
                                                                                                                                       && ((s
                                                                                                                                             /= d)
                                                                                                                                            && (leftdigit
                                                                                                                                                 m
                                                                                                                                                 && ((m
                                                                                                                                                       /= s)
                                                                                                                                                      && ((m
                                                                                                                                                            /= o)
                                                                                                                                                           && ((m
                                                                                                                                                                 /= r)
                                                                                                                                                                && ((m
                                                                                                                                                                      /= n)
                                                                                                                                                                     && ((m
                                                                                                                                                                           /= y)
                                                                                                                                                                          && ((m
                                                                                                                                                                                /= e)
                                                                                                                                                                               && ((m
                                                                                                                                                                                     /= d)
                                                                                                                                                                                    && sumdigit
                                                                                                                                                                                        c3
                                                                                                                                                                                        s
                                                                                                                                                                                        m
                                                                                                                                                                                        o
                                                                                                                                                                                        m)))))))))))))))))))))))))))))))))))
  = [s,e,n,d,m,o,r,y]
  where
    c1, c2, c3, s, e, n, d, m, o, r, y free

sumdigit c a b s d | sumdigitS ((c + a) + b) s d = True

sumdigitS x s d =
  if x < 10
     then ((s =:= x) && (d =:= 0)) &> True
     else ((s =:= (x - 10)) && (d =:= 1)) &> True

digit 0 = True
digit 1 = True
digit 2 = True
digit 3 = True
digit 4 = True
digit 5 = True
digit 6 = True
digit 7 = True
digit 8 = True
digit 9 = True

leftdigit 1 = True
leftdigit 2 = True
leftdigit 3 = True
leftdigit 4 = True
leftdigit 5 = True
leftdigit 6 = True
leftdigit 7 = True
leftdigit 8 = True
leftdigit 9 = True

main = smm
