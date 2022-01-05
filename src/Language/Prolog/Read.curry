------------------------------------------------------------------------------
--- This library contains operations to read/parse Prolog programs.
---
--- @author Michael Hanus
--- @version January 2022
------------------------------------------------------------------------------
{-# OPTIONS_FRONTEND -Wno-incomplete-patterns -Wno-overlapping #-}

module Language.Prolog.Read
  ( readPrologFile, parseProlog )
 where

import Data.Char ( isAlphaNum, isLower, isSpace, isUpper )
import Data.List ( union, intercalate )

import Control.SetFunctions

import Language.Prolog.Show  ( showPlProg, showPlClause, showPlTerm )
import Language.Prolog.Types

{-
-- Some testing:

--main1 = parseTerm False [] (scanPl "f(x23,Y)")
main2 = scanPl "f(x+y)"
main3 = scanPl "isColor(blue).\nisColor(red)."
main4 = parseProlog "?-(f(X))."
main5 = ppProg "isColor(blue).\nisColor(red).\n?- isColor(42)."
main6 = ppProg "%CMT\n:- module(a).\np(X) :- p(f(X)).\np(true).\n?- p(Z).\n"
main7 = parseProlog ":- op(400, xfy, ++).\n1 ++ 2 ++ 3.\n"
main8 = parseProlog ":- op(400, yfx, [++, +-]).\n1 ++ 2 ++ 3.\n"
main9 = parseProlog "rev([1,2])."

-- Reads Prolog file and print it.
readProg :: String -> IO ()
readProg f = readPrologFile f >>= putStr . showPlProg

ppProg :: String -> IO ()
ppProg = putStr . showPlProg . parseProlog
-}

----------------------------------------------------------------------------
-- The state used during parsing contains the currently defined operators
-- (which are changed by reading an `op` directive).

data ParseState = ParseState
       { opDecls :: [(String,Int,String)] -- operator name / priority / fixity
       }

initParseState :: ParseState
initParseState = ParseState stdOps

----------------------------------------------------------------------------

--- Reads a file containing a Prolog program and return the Prolog clauses.
readPrologFile :: String -> IO [PlClause]
readPrologFile f = readFile f >>= return . parseProlog

--- Parses a string containing a list of Prolog clauses which are returned.
parseProlog :: String -> [PlClause]
parseProlog = parseClauses initParseState . scanPl

-- Parses a sequence of tokens as Prolog clauses where each clause
-- is terminated by a dot.
parseClauses :: ParseState -> [PlToken] -> [PlClause]
parseClauses _   []       = []
parseClauses pst ts@(_:_) = case ts1 of
  DOT : ts2 -> let cl = term2clause term
               in cl : parseClauses (addOpDecl cl pst) ts2
  _         -> error $ "Missing terminating dot at: " ++ show ts1
 where
  (term, ts1) = parseTerm False pst [] ts

-- Adds operators to the state if the clause is an `op` directive.
addOpDecl :: PlClause -> ParseState -> ParseState
addOpDecl clause pst = case clause of
  PlDirective [PlLit "op" opargs] -> case opargs of
    [PlInt prio, PlAtom fix, ops] -> case ops of
      PlAtom op -> addOps [(op, prio, fix)]
      opsterm   -> addOps (map (\o -> (o, prio, fix)) (opList opsterm))
    _ -> error opError
  _ -> pst
 where
  opList ops = case ops of
    PlStruct "." [PlAtom op, os] -> op : opList os
    PlAtom "[]"                  -> []
    _                            -> error opError
    
  opError = "Illegal op directive: " ++ showPlClause clause

  addOps ops = pst { opDecls = opDecls pst ++ ops }

-- Translate a Prolog term into a clause by interpreting the main operators.
term2clause :: PlTerm -> PlClause
term2clause term = case term of
    PlStruct f [arg] | f == "?-" -> PlQuery     (term2goals arg)
    PlStruct f [arg] | f == ":-" -> PlDirective (term2goals arg)
    PlStruct f [l,r] | f == ":-" -> let PlLit p args = term2goal l
                                    in PlClause p args (term2goals r)
    PlStruct f args -> PlClause f args []
    PlAtom   f      -> PlClause f []   []
    _               -> error $ "Illegal clause: " ++ showPlTerm term
 where
  term2goals t = case t of
    PlStruct "," [a1,a2] -> term2goal a1 : term2goals a2
    _                    -> [term2goal t]

  term2goal t = case t of
    PlStruct "\\+" [arg] -> PlNeg (term2goals arg)
    PlStruct ";" [PlStruct "->" [cnd,thn],els] ->
      PlCond (term2goals cnd) (term2goals thn) (term2goals els)
    PlStruct f args -> PlLit f args
    PlAtom   f      -> PlLit f []
    _               -> error $ "Illegal goal: " ++ showPlTerm t

-- Parses a sequence of Prolog terms (possibly containing operators).
-- The second argument contains the previously parsed terms of this list.
-- If the first argument is `True`, the term is terminated by a comma,
-- otherwise, a comma is parsed as an infix operator.
parseTerm :: Bool -> ParseState -> [PlTerm] -> [PlToken] -> (PlTerm, [PlToken])
parseTerm stopcomma pst pts ts = case ts of
  VAR s     : ts1 -> parseTerm stopcomma pst (PlVar s : pts) ts1
  INT n     : ts1 -> parseTerm stopcomma pst (PlInt n : pts) ts1
  FUNCTOR s : ts1 ->
    let (args,ts2) = parseCommaTerms pst [] ts1
    in case ts2 of
         RPAREN : ts3 -> parseTerm stopcomma pst (PlStruct s args : pts) ts3
         _            -> rightBracketError ts2
  ATOM s : ts1 -> parseTerm stopcomma pst (PlAtom s : pts) ts1
  COMMA  : ts1 | not stopcomma -> parseTerm stopcomma pst (PlAtom "," : pts) ts1
  LPAREN : ts1 ->
    let (term, ts2) = parseTerm False pst [] ts1
    in case ts2 of RPAREN : ts3 -> parseTerm stopcomma pst (term : pts) ts3
                   _            -> rightBracketError ts2
  LSQUARE : ts1 -> let (listterm, ts2) = parseList pst ts1
                   in parseTerm stopcomma pst (listterm : pts) ts2
  _             -> (terms2term pst pts, ts)
 where
  rightBracketError toks = error $ "Missing right bracket at: " ++ show toks

-- Parses a list of comma-separated Prolog terms.
parseCommaTerms :: ParseState -> [PlTerm] -> [PlToken] -> ([PlTerm], [PlToken])
parseCommaTerms pst args ts =
  let (term, ts1) = parseTerm True pst [] ts
  in case ts1 of
       COMMA  : ts2 -> parseCommaTerms pst (term:args) ts2
       _            -> (reverse (term:args), ts1)

-- Parses a list in Prolog notation (left square bracket already consumed).
parseList :: ParseState -> [PlToken] -> (PlTerm, [PlToken])
parseList pst ts =
  let (terms, ts1) = parseCommaTerms pst [] ts
  in case ts1 of
       RSQUARE : ts2 -> (consList terms (PlAtom "[]"), ts2)
       BAR     : ts2 ->
         let (tail, ts3) = parseTerm True pst [] ts2
         in case ts3 of RSQUARE : ts4 -> (consList terms tail, ts4)
                        _             -> listError ts3
       _             -> listError ts1
 where
  listError toks = error $ "Non-terminated list at: " ++ show toks

  consList []     tail = tail
  consList (x:xs) tail = PlStruct "." [x, consList xs tail]


-- Combines a list of terms (provided as first argument in reverse order)
-- into a single term by considering Prolog operator precedences.
terms2term :: ParseState -> [PlTerm] -> PlTerm
terms2term pst ts =
  if notEmpty terms
    then snd (selectValue terms) -- also check for more than one solution...
    else error $ "Illegal term: " ++ unwords (map showPlTerm (reverse ts))
 where terms = set2 opterms2term pst (reverse ts)

opterms2term :: ParseState -> [PlTerm] -> (Int, PlTerm)
opterms2term _   [t] = (0,t)
opterms2term pst (PlAtom f : ts)
  | isPrefixOp fprio fa tp = (fprio, PlStruct f [t])
 where (fprio,fa) = lookupOpInfo f (opDecls pst)
       (tp,t) = opterms2term pst ts
opterms2term pst (ts ++ [PlAtom f])
  | isPostfixOp fprio fa tp = (fprio, PlStruct f [t])
 where (fprio,fa) = lookupOpInfo f (opDecls pst)
       (tp,t) = opterms2term pst ts
opterms2term pst (ts1 ++ [PlAtom f] ++ ts2)
  | isInfixOp fprio fa tp1 tp2 = (fprio, PlStruct f [t1,t2])
 where (fprio,fa) = lookupOpInfo f (opDecls pst)
       (tp1,t1) = opterms2term pst ts1
       (tp2,t2) = opterms2term pst ts2

isPrefixOp :: Int -> String -> Int -> Bool
isPrefixOp fprio fa tprio = fa == "fx" && tprio < fprio ||
                            fa == "fy" && tprio <= fprio

isPostfixOp :: Int -> String -> Int -> Bool
isPostfixOp fprio fa tprio = fa == "xf" && tprio < fprio ||
                             fa == "yf" && tprio <= fprio

isInfixOp :: Int -> String -> Int -> Int -> Bool
isInfixOp fprio fa tp1 tp2 = fa == "xfx" && tp1 <  fprio && tp2 < fprio ||
                             fa == "yfx" && tp1 <= fprio && tp2 < fprio ||
                             fa == "xfy" && tp1 <  fprio && tp2 <= fprio

lookupOpInfo :: String -> [(String,Int,String)] -> (Int,String)
lookupOpInfo op ((op,prio,a):_) = (prio,a)
lookupOpInfo op (_:ops)         = lookupOpInfo op ops

------------------------------------------------------------------------------
-- A scanner for Prolog.

data PlToken =
    DOT -- a single dot (end of term)
  | LPAREN  | RPAREN
  | LSQUARE | RSQUARE | BAR
  | COMMA
  | VAR     String
  | FUNCTOR String  -- a functor, i.e., directly followed by an open bracket
  | ATOM    String  -- an atom (not followed by an open bracket)
  | INT     Int
 deriving Show

scanPl :: String -> [PlToken]
scanPl s = case dropWhile isSpace s of
  []         -> []
  '%':cs     -> scanPl (dropWhile (/='\n') cs) -- skip comment line
  '(':cs     -> LPAREN : scanPl cs
  ')':cs     -> RPAREN : scanPl cs
  ',':cs     -> COMMA  : scanPl cs
  ';':cs     -> ATOM ";" : scanPl cs
  '!':cs     -> ATOM "!" : scanPl cs
  '\'':cs    -> scanTickAtom "" cs
  '.':cs     -> if null cs || isSpace (head cs)
                  then DOT : scanPl cs
                  else scanOp "." cs
  '[':']':cs -> ATOM "[]" : scanPl cs
  '[':cs     -> LSQUARE : scanPl cs
  ']':cs     -> RSQUARE : scanPl cs
  '|':cs     -> BAR     : scanPl cs
  c:cs | isLower c             -> scanAtom [c] cs
       | isUpper c || c == '_' -> scanVar  [c] cs
       | isPrologSpecial c     -> scanOp   [c] cs
       | isDigit c             -> scanNum  [c] cs
  cs -> error $ "Scan error: " ++ cs

scanAtom :: String -> String -> [PlToken]
scanAtom pre [] = [ATOM (reverse pre)]
scanAtom pre s@(c:cs)
  | isAlphaNum c || c == '_' = scanAtom (c:pre) cs
  | c == '('                 = FUNCTOR (reverse pre) : scanPl cs
  | otherwise                = ATOM (reverse pre) : scanPl s

scanTickAtom :: String -> String -> [PlToken]
scanTickAtom pre [] = error $ "Scan error: non-terminated atom: " ++ pre
scanTickAtom pre s@(c:cs)
  | s == "\\" = error $ "Scan error: non-terminated atom: " ++ pre
  | c == '\\' && head cs == '\'' = scanTickAtom (head cs : pre) cs
  | c == '\''                    = ATOM (reverse pre) : scanPl cs
  | otherwise                    = scanTickAtom (c : pre) cs

scanVar :: String -> String -> [PlToken]
scanVar pre [] = [VAR (reverse pre)]
scanVar pre s@(c:cs)
  | isAlphaNum c || c == '_' = scanVar (c:pre) cs
  | otherwise                = VAR (reverse pre) : scanPl s

scanOp :: String -> String -> [PlToken]
scanOp pre [] = [ATOM (reverse pre)]
scanOp pre s@(c:cs)
  | isPrologSpecial c = scanOp (c:pre) cs
  | c == '('          = FUNCTOR (reverse pre) : scanPl cs
  | otherwise         = ATOM (reverse pre) : scanPl s

scanNum :: String -> String -> [PlToken]
scanNum pre [] = [INT (read (reverse pre))]
scanNum pre s@(c:cs)
  | isDigit c = scanNum (c:pre) cs
  | otherwise = INT (read (reverse pre)) : scanPl s

isPrologSpecial :: Char -> Bool
isPrologSpecial = (`elem` "+-*/<=>`\\:.?@#$&^~")

----------------------------------------------------------------------------

-- The standard operators used in Prolog programs.
stdOps :: [(String,Int,String)]
stdOps =
  [ (":-",1200,"xfx")
  , ("-->",1200,"xfx")
  , (":-",1200,"fx")
  , ("?-",1200,"fx")
  , (";",1100,"xfy")
  , ("dynamic",1150,"fx")
  , ("function",1150,"fx")   -- to specify a function (with result positions)
  , ("->",1050,"xfy")
  , (",",1000,"xfy")
  , ("\\+",900,"fy")
  , ("is",700,"xfx")
  , ("=",700,"xfx"), ("\\=",700,"xfx"), ("=:=",700,"xfx"), ("=\\=",700,"xfx")
  , ("=..",700,"xfx")
  , ("=<",700,"xfx"), (">=",700,"xfx"), ("<",700,"xfx"), (">",700,"xfx")
  , (":",550,"xfy")
  , ("+",500,"yfx"), ("-",500,"yfx")
  , ("*",400,"yfx"), ("/",400,"yfx")
  , ("//",400,"yfx")
  , ("mod",400,"yfx")
  , ("rem",400,"yfx")
  , ("div",400,"yfx")
  , ("**",200,"xfx")
  , ("^",200,"xfy")
  , ("-",200,"fy")
  , ("+",200,"fy")
  ]

{-
Get all currently defined operators in Prolog with:

?- current_op(X,Y,Z), write('("'), write(Z), write('",'), write(X), write(',"'),write(Y), write('")'), nl, fail.

-}

----------------------------------------------------------------------------
