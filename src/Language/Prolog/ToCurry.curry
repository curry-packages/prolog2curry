------------------------------------------------------------------------------
--- This library contains operations to read/parse Prolog programs.
---
--- @author Michael Hanus
--- @version January 2022
------------------------------------------------------------------------------

module Language.Prolog.ToCurry
  ( TransState(..), initState, setModName
  , showIndSeqArgs, showResultArgs
  , prolog2Curry )
 where

import Data.Char ( toLower, toUpper )
import Data.List ( (\\), find, intersect, isSuffixOf, last, maximum
                 , minimum, nub, partition, transpose, union )
import System.IO.Unsafe ( trace )

import AbstractCurry.Build
import AbstractCurry.Pretty  ( showCProg )
import AbstractCurry.Types

import Language.Prolog.Read  ( readPrologFile )
import Language.Prolog.Show  ( showPlClause, showPlGoal, showPlGoals
                             , showPlProg )
import Language.Prolog.Types


-- Reads Prolog program from a file (with suffix `.pl`)
-- and print the transformed program.
transProg :: String -> IO ()
transProg pname = do
  pp <- readPrologFile (pname ++ ".pl")
  putStrLn $ showPlProg pp
  let currymod    = upperFirst pname
      ts          = initState currymod
      (cprog,ts1) = prolog2Curry ts pp
      ucprog   = unlines (filter (not . (":: ()" `isSuffixOf`))
                                 (lines (showCProg cprog)))
  putStrLn ("Functions used in the transformation:\n" ++ showResultArgs ts1)
  putStrLn ucprog
  writeFile (currymod ++ ".curry") ucprog
  putStrLn $ "Written into '" ++ currymod ++ ".curry'"

----------------------------------------------------------------------------
--- A predicate specification consists of a name and an arity.
type PredSpec = (String,Int)

--- A clause defining a predicate is a pair of the pattern list and the
--- list of goals in the body.
type Clause = ([PlTerm],[PlGoal])

--- The state used during the transformation.
--- Apart from various options to control the transformation,
--- it contains the list of predicates and constructors used in the
--- Prolog program.

data TransState = TransState
  { modName       :: String   -- name of the generated Curry module
  , optVerb       :: Int      -- verbosity
                              -- (0: quiet, 1: status, 2: intermediate, 3: all)
  , optHelp       :: Bool     -- if help info should be printed
  , optOutput     :: String   -- name of output file (or "-")
  , optLoad       :: Bool     -- load the generated Curry module
  , optNoWarn     :: Bool     -- turn off warnings for generated Curry module
  , withFunctions :: Bool     -- use function information (otherwise,
                              -- use conservative transformation)
  , withDemand    :: Bool     -- transform to exploit demand/lazy
                              -- evaluation, i.e., use let bindings
  , optAnyResult  :: Bool     -- allow any position as result
                              -- (i.e., not only the last one)
  , withInline    :: Bool     -- try to inline where/let bindings
  , useLists      :: Bool     -- translate Prolog lists into Curry lists?
  , useAnalysis   :: Bool     -- derive function information automatically

  -- the following components are automatically set by the transformation:
  , ignoredCls    :: [PlClause]          -- ignored clauses (queries, direct.)
  , prologPreds   :: [(PredSpec,String)] -- predicate spec and output name
  , prologCons    :: [(String,Int)]      -- structure name / arity
  , indseqArgs    :: [(PredSpec,[[Int]])]-- min. sets. of ind. seq. arg. posns.
  , resultArgs    :: [(PredSpec,[Int])]  -- result argument positions
  }

--- Returns an initial transformation state for a given module name.
initState :: String -> TransState
initState mname =
  TransState mname 1 False "" False False True True False True True True
             [] [] [] [] initResultArgs
 where
  initResultArgs = [(("is",2),[1])]

--- Sets the name of the generated Curry module from the given Prolog name.
setModName :: String -> TransState -> TransState
setModName pl ts = ts { modName = upperFirst pl }

updatePredName :: PredSpec -> String -> TransState -> TransState
updatePredName pnar newpn ts = ts { prologPreds = updName (prologPreds ts) }
 where
  updName [] = []
  updName ((pa,n) : pas) | pa == pnar = (pa,newpn) : updName pas
                         | otherwise  = (pa,n)     : updName pas

-- Looks up the inductively sequential argument positions for a predicate
-- in a transformation state.
indseqPos :: TransState -> PredSpec -> [[Int]]
indseqPos ts pnar = maybe [] id (lookup pnar (indseqArgs ts))

-- Looks up result arguments for a predicate in a transformation state.
resultPos :: TransState -> PredSpec -> [Int]
resultPos ts pnar = maybe [] id (lookup pnar (resultArgs ts))

showPredInfo :: (a -> String) -> [(PredSpec,a)] -> String
showPredInfo showi =
  unlines . map (\ (pnar,info) -> showPredArity pnar ++ ": " ++ showi info)

showPredPositions :: [(PredSpec,[Int])] -> String
showPredPositions = showPredInfo (unwords . map show)

showIndSeqArgs :: TransState -> String
showIndSeqArgs ts = showPredInfo (unwords . map show) (indseqArgs ts)

showResultArgs :: TransState -> String
showResultArgs ts = showPredPositions (resultArgs ts)

showPredArity :: PredSpec -> String
showPredArity (pn,ar) = pn ++ "/" ++ show ar

----------------------------------------------------------------------------

--- Translates a list of Prolog clauses into an AbstractCurry program
--- and return also the modified translation state.
prolog2Curry :: TransState -> [PlClause] -> (CurryProg, TransState)
prolog2Curry ts cls =
  let (functiondirs, cls1)  = extractFunctonDirectives cls
      (predclauses,ignored) = sortPredicates cls1
      allconstrsP = unionMap patsrhsconstrs (concatMap snd predclauses) \\
                      stdConstrs
      allconstrs = if useLists ts then allconstrsP \\ [("[]",0), (".",2)]
                                  else allconstrsP
      ts1 = ts { ignoredCls  = ignored
               , prologPreds = map (\ ((pn,ar),_) -> ((pn,ar),pn)) predclauses
               , prologCons  = allconstrs
               , resultArgs  = resultArgs ts ++ functiondirs }
      ts2 = if useAnalysis ts && withFunctions ts
              then analyzeClauses predclauses ts1
              else ts1
  in (simpleCurryProg (modName ts) ["Prelude"]
        (if null allconstrs then [] else [constrs2type ts allconstrs])
        (map (transPredClauses ts2) predclauses) [],
      ts2)
 where
  patsrhsconstrs (pats,goals) =
    union (unionMap termConstrs pats) (unionMap goalConstrs goals)

  stdConstrs = map (\o -> (o,2)) ["+","-","*","/"]

----------------------------------------------------------------------------
-- Analyze the predicates defined with the given list of clauses
-- and store the analysis results in the state.
analyzeClauses :: [(PredSpec, [Clause])] -> TransState -> TransState
analyzeClauses cls ts = analyzeFunctions cls (analyzeIndSeqArgs cls ts)

-- Derive `function` directives for all predicates defined in the given
-- list of clauses. Already existing directives are not changed.
analyzeFunctions :: [(PredSpec, [Clause])] -> TransState -> TransState
analyzeFunctions []                                  ts = ts
analyzeFunctions ((pnar@(pn,ar),pcls) : predclauses) ts =
  maybe (let fspecs = resultArgs ts
             ps     = computeResults fspecs pcls
             ts1    = if null ps
                        then ts
                        else ts { resultArgs = fspecs ++ [(pnar,ps)] }
             ts2    = case ps of -- change predicate name of result is not last
                        [p] | p/=ar -> updatePredName pnar (pn ++ '_' : show p)
                                                      ts1
                        _           -> ts1
         in analyzeFunctions predclauses ts2)
        (const $ analyzeFunctions predclauses ts) -- keep existing func. specs.
        (lookup pnar (resultArgs ts))
 where
  computeResults funcspecs cls
    | ar == 0
    = []   -- no result position
    | length cls == 1 -- defined by single rule:
      -- if the last argument is a non-variable or a variable defined in
      -- a result position, we interpret this predicate as a function:
    = case last (fst (head cls)) of
        PlVar v -> if isResultVar funcspecs v (snd (head cls)) then [ar]
                                                               else []
        _       -> [ar]
    | not (null indseqpos) -- defined by non-overlapping patterns
    = if optAnyResult ts
        then if ar == 1 then [] -- or non-deterministic operation?
                        else [maximum ([1 .. ar] \\ indseqpos)]
        else if null (indseqpos \\ [ar]) then [] else [ar]
    | otherwise
    = []
   where
    allindseqpos = indseqPos ts (pn,ar)
    resindseqpos = if optAnyResult ts then allindseqpos
                                      else filter (ar `notElem`) allindseqpos
    indseqpos = if null resindseqpos then [] else head resindseqpos

-- Analyze the inductively sequential argument positions
-- (i.e., groups of arguments which are inductively sequential)
-- for all predicates defined in the given list of clauses and add the
-- analysis results to the state.
analyzeIndSeqArgs :: [(PredSpec, [Clause])] -> TransState -> TransState
analyzeIndSeqArgs []                          ts = ts
analyzeIndSeqArgs ((pnar,pcls) : predclauses) ts =
  maybe (let ps  = computeIndSeqArgs pcls
             ts1 = if null ps
                     then ts
                     else ts { indseqArgs = indseqArgs ts ++ [(pnar,ps)] }
         in analyzeIndSeqArgs predclauses ts1)
        (const $ analyzeIndSeqArgs predclauses ts) -- keep existing i.seq. args
        (lookup pnar (indseqArgs ts))
 where
  computeIndSeqArgs cls = groupOfIndSeqArgs (map (zip [1 ..]) (map fst cls))

-- Infer a minimal sets of inductively sequential argument positions
-- for all predicates defined in the given list of clauses and add the
-- analysis results to the state.
-- These sets are all minimal w.r.t. its size, i.e., all lists in the
-- lists of results have the same length.
groupOfIndSeqArgs :: [[(Int,PlTerm)]] -> [[Int]]
groupOfIndSeqArgs rows
  | null rows             = [] -- no rows
  | null (head rows)      = [] -- no pattern columns
  | not (null uniquecols) = map ((:[]) . fst . head) uniquecols -- uniqe columns
  | null conscols         = [] -- no pattern column with constructors only
  | null iseqconscols     = [] -- no ind. seq. constructor columns
  | otherwise  
  = let minlen = minimum (map (length . head) iseqconscols)
    in concatMap (filter (\xs -> length xs <= minlen)) iseqconscols
 where
  patcols = transpose rows -- the pattern columns

  uniquecols = filter (\c -> nonOverlappingConsTerms (map snd c)) patcols

  -- pattern columns where all patterns are non-variables
  conscols = filter (\ (c:_) -> all (not . isPlVar) (map snd c))
                    (splitList patcols)

  -- ind.seq. positions w.r.t. each non-variable pattern column
  iseqconscols = filter (not . null) (map indseqArgsOfCC conscols)

  indseqArgsOfCC allcols@(cc : _) =
    if any null iseqrootrows
      then []
      else [nub (fst (head cc) : concatMap head iseqrootrows)]
   where
    roots = nub (map rootOf (map snd cc))

    withRoot []                      _ = [] -- no more rows
    withRoot (((i,pat):pats) : rs) s
      | rootOf pat == s = (zip (repeat i) (argsOf pat) ++ pats) : withRoot rs s
      | otherwise       = withRoot rs s

    -- group rows according to identical root patterns:
    rootRows = filter (\rs -> length rs > 1)
                       (map (withRoot (transpose allcols)) roots)

    iseqrootrows = map groupOfIndSeqArgs rootRows

-- Splits a list into a list of each element followed by the other elements.
-- E.g., `split [1,2,3] == [[1,2,3], [2,1,3], [3,1,2]]
splitList :: [a] -> [[a]]
splitList = split []
 where
  split _  []     = []
  split ys (x:xs) = (x : reverse ys ++ xs) : split (x:ys) xs


-- Is a list of terms pairwise disjoint and constructor-rooted?
nonOverlappingConsTerms :: [PlTerm] -> Bool
nonOverlappingConsTerms []     = True
nonOverlappingConsTerms (p:ps) =
  not (isPlVar p) && all (disjointTerms p) ps && nonOverlappingConsTerms ps

-- Are two terms disjoint?
disjointTerms :: PlTerm -> PlTerm -> Bool
disjointTerms t1 t2 = case t1 of
  PlVar _       -> False
  PlInt i       -> case t2 of PlVar _   -> False
                              PlInt j   -> i /= j
                              _         -> True
  PlFloat x     -> case t2 of PlVar   _ -> False
                              PlFloat y -> x /= y
                              _         -> True
  PlAtom a      -> case t2 of PlVar _   -> False
                              PlAtom b  -> a /= b
                              _         -> True
  PlStruct s xs -> case t2 of
                     PlVar _       -> False
                     PlStruct t ys -> s /= t ||
                                      any (uncurry disjointTerms) (zip xs ys)
                     _             -> True

-- Is a variable defined in a result argument position in a Prolog goal?
isResultVar :: [(PredSpec,[Int])] -> String -> [PlGoal] -> Bool
isResultVar fspecs lvar goals = any isResultVarInGoal goals
 where
  isResultVarInGoal goal = case goal of
    PlNeg  _       -> False -- no analysis for negation so far
    PlCond _ tr fl -> isResultVar fspecs lvar tr || isResultVar fspecs lvar fl
    PlLit  pn args -> let rpos = maybe [] id (lookup (pn, length args) fspecs)
                          (res,_) = partitionPredArguments rpos args
                      in lvar `elem` termsVars res

----------------------------------------------------------------------------
-- Translates a list of constructors into a data declaration.
constrs2type :: TransState -> [(String,Int)] -> CTypeDecl
constrs2type ts cs =
  CType termType Public [] (map c2cdecl cs) (map pre ["Eq", "Show"])
 where
  termType = (modName ts, "Term")

  c2cdecl (c,i) =
    CCons (transName ts c) Public (map (const (baseType termType)) [1 .. i])

-- Extracts `function` directives from a list of Prolog clauses.
-- Returns the remaining clauses and the function specifications.
extractFunctonDirectives :: [PlClause] -> ([(PredSpec,[Int])], [PlClause])
extractFunctonDirectives cls =
  let (functiondirs, othercls) = partition isFunctionDirective cls
  in (map dir2spec functiondirs, othercls)
 where
  isFunctionDirective cl = case cl of
    PlDirective [PlLit "function" _] -> True
    _                                -> False

  dir2spec cl = case cl of
    PlDirective [PlLit _ [PlStruct ":" [pspec,rspec]]] ->
                                     (getPredSpec pspec, getResultPos rspec)
    PlDirective [PlLit _ [pspec]] ->
      let (pred,ar) = getPredSpec pspec in ((pred,ar), [ar])
    _ -> error "Internal error: extractFunctonDirectives"
   where
    getPredSpec t = case t of
      PlStruct "/" [PlAtom pred, PlInt ar] -> (pred,ar)
      _ -> error $ "Illegal predicate specification in: " ++ showPlClause cl

    getResultPos t = case t of
      PlInt p -> [p]
      _       -> getResultPosList t

    getResultPosList t = case t of
      PlAtom "[]"                -> []
      PlStruct "." [PlInt p, ts] -> p : getResultPosList ts
      _ -> error $ "Illegal function directive: " ++ showPlClause cl

-- Sorts a list of Prolog clauses into a list of Prolog clauses
-- for each predicate. All directives and queries are ignored and
-- returned in the second component.
sortPredicates :: [PlClause] -> ([(PredSpec, [Clause])], [PlClause])
sortPredicates []       = ([],[])
sortPredicates (cl:cls) = case cl of
  PlDirective _ -> let (pcls,icls) = sortPredicates cls in (pcls, cl:icls)
  PlQuery     _ -> let (pcls,icls) = sortPredicates cls in (pcls, cl:icls)
  PlClause pn args goals ->
    let ar = length args
        (pnclauses,othercls) = partition (hasNameArity pn ar) cls
        (pcls,icls)          = sortPredicates othercls
    in (((pn,ar), (args,goals) : map patsRhs pnclauses) : pcls, icls)
 where
  hasNameArity _ _  (PlDirective _     ) = False
  hasNameArity _ _  (PlQuery     _     ) = False
  hasNameArity n ar (PlClause pn args _) = n == pn && ar == length args

  patsRhs clause = case clause of
    PlClause _ args goals -> (args,goals)
    _                     -> error "Internal error at sortPredicates.patsRhs"

----------------------------------------------------------------------------
-- The actual transformation functions.

-- Translate all clauses for a predicate into a Curry function.
transPredClauses :: TransState -> (PredSpec, [Clause])
                 -> CFuncDecl
transPredClauses ts ((pn,ar), clauses) =
  cfunc (transName ts pn) ar Public
        (emptyClassType unitType) -- dummy type, will be removed
        (map (transClause ts (pn,ar)) clauses)

-- Translate a Prolog clause into a Curry rule.
transClause :: TransState -> PredSpec -> Clause -> CRule
transClause ts (pn,ar) (args, goals)
  | withFunctions ts = trClauseFunctional   ts (pn,ar) args goals
  | otherwise        = trClauseConservative ts args goals

-- Translate a Prolog clause into a Curry rule with the functional
-- transformation, i.e., consider the information about result positions.
-- A conditional clause `c -> g1 ; g2` is translated into if-then-else.
trClauseFunctional :: TransState -> PredSpec -> [PlTerm] -> [PlGoal]
                   -> CRule
trClauseFunctional ts pnar predargs goals = case goals of
  [PlCond [PlLit cp cts] g1 g2] -> -- handling of if-then-else rules:
       let (guard1,rhs1,binds1) = transGoals ts argvars g1 inrhs
           (guard2,rhs2,binds2) = transGoals ts argvars g2 inrhs
           extravars1 = unionMap termVars [guard1, rhs1] \\
                          (argvars ++ termsVars (concatMap fst binds1))
           extravars2 = unionMap termVars [guard2, rhs2] \\
                          (argvars ++ termsVars (concatMap fst binds2))
       in simpleRule patterns
                     (cITE (transTerm ts (PlStruct (checkCond goals cp) cts))
                           (letExpr (bindsvars2local binds1 extravars1)
                                    (condExp guard1 rhs1))
                           (letExpr (bindsvars2local binds2 extravars2)
                                    (condExp guard2 rhs2)))
  _ -> let (guard,rhs,binds) = transGoals ts argvars goals inrhs
           extravars = unionMap termVars [guard, rhs] \\
                         (argvars ++ termsVars (concatMap fst binds))
       in guardedRule patterns
                      [(transTerm ts guard, transTerm ts rhs)]
                      (bindsvars2local binds  extravars)
 where
  bind2local (pts,e) = CLocalPat (tuplePattern (map (transPattern ts) pts))
                                 (CSimpleRhs (transTerm ts e) [])

  -- translate bindings and free variable into local declarations
  bindsvars2local binds freevars =
    let fvars = filter (/="_") freevars
    in  map bind2local binds ++
        if null fvars then []
                      else [CLocalVars (map (\v -> (1, lowerFirst v)) fvars)]

  -- generate conditional expression of the form `c &> e`
  condExp c e = if c == plTrue
                  then transTerm ts e
                  else applyF (pre "&>") (map (transTerm ts) [c,e])

  (res,args) = partitionPredArguments (resultPos ts pnar) predargs
  inrhs      = if null res then plTrue else tupleTerm res
  argvars    = termsVars args
  patterns   = map (transPattern ts) args

-- Translates a Prolog clause into a Curry rule with the convervative
-- transformation scheme, i.e., translates a Prolog predicate into a
-- Curry predicate.
-- A conditional clause `c -> g1 ; g2` is translated into if-then-else.
trClauseConservative :: TransState -> [PlTerm] -> [PlGoal] -> CRule
trClauseConservative ts args goals = case goals of
  [PlCond [PlLit cp cts] g1 g2] ->
       let (guard1,_,_) = transGoals ts [] g1 plTrue
           (guard2,_,_) = transGoals ts [] g2 plTrue
       in guardedRule patterns
                      [(constF (pre "True"),
                        cITE (transTerm ts (PlStruct (checkCond goals cp) cts))
                             (transTerm ts guard1)
                             (transTerm ts guard2))]
                     localvars
  _ -> let (guard,rhs,_) = transGoals ts [] goals plTrue
       in guardedRule patterns [(transTerm ts guard, transTerm ts rhs)]
                      localvars
 where
  patterns  = map (transPattern ts) args
           
  extravars = filter (/="_") (unionMap goalVars goals \\ termsVars args)
  localvars = if null extravars
                then []
                else [CLocalVars (map (\v -> (1, lowerFirst v)) extravars)]

-- Checks a condition predicate. If it not a simple one, raise an error.
checkCond :: [PlGoal] -> String -> String
checkCond goals cp =
  if cp `elem` simpleCmpPreds
    then cp
    else trace ("WARNING: conditional with complex condition occurred:\n" ++
                 showPlGoals goals)
               cp


-- Translates a Prolog term into a Curry pattern.
transPattern :: TransState -> PlTerm -> CPattern
transPattern ts pterm = case pterm of
  PlVar v       -> cpvar (lowerFirst v)
  PlInt i       -> pInt i
  PlFloat i     -> pFloat i
  PlAtom a      -> CPComb (transName ts a) []
  PlStruct s ps -> CPComb (transName ts s) (map (transPattern ts) ps)

-- Translates a list of Prolog goals and a term representing the rhs term
-- into a term representing the goal as a Boolean condition,
-- a term representing the (possibly transformed rhs),
-- and local bindings in case of the demand transformation.
-- The second argument contains the lhs variables
-- in order to avoid creating new bindings for them.
transGoals :: TransState -> [String] -> [PlGoal] -> PlTerm
           -> (PlTerm, PlTerm,[([PlTerm], PlTerm)])
transGoals ts lvars goals rhs =
  let (guardexps,binds)  = unzip (map (transGoal ts lvars) goals)
      (ubinds,multbinds) = partition (null . tail)
                                     (groupBindings (concat binds))
      -- translate multiple bindings for a same variable into unifications:
      multbindsguards    = map bind2Equ (concat multbinds)
      guardexp           = if null (concat guardexps ++ multbindsguards)
                             then plTrue
                             else foldr1 (\t1 t2 -> PlStruct "&&" [t1,t2])
                                         (concat guardexps ++ multbindsguards)
  in if withInline ts
       then let (sbinds, [sguardexp, srhs]) =
                   substBindings [] (concat ubinds) [guardexp, rhs]
            in (sguardexp, srhs, sbinds)
       else (guardexp, rhs, concat ubinds)
 where
  bind2Equ (res, call) = PlStruct "=:=" [tupleTerm res, call]

  -- transform bindings into groups 
  groupBindings [] = []
  groupBindings ((l,r):bs) =
    let (nols,ls) = partition (\b -> null (intersect (termsVars (fst b))
                                                     (termsVars l)))
                              bs
    in ((l,r):ls) : groupBindings nols

-- Translates a Prolog goal into a term (representing the goal
-- as a Boolean condition) with local bindings in case of the demand
-- transformation. The second argument contains the lhs variables
-- in order to avoid creating new bindings for them.
transGoal :: TransState -> [String] -> PlGoal
          -> ([PlTerm], [([PlTerm], PlTerm)])
transGoal _ _ goal@(PlNeg _) =
  error $ "Cannot translate negation: " ++ showPlGoal goal
transGoal _ _ goal@(PlCond _ _ _) =
  error $ "Cannot translate non-top-level conditional: " ++ showPlGoal goal
transGoal ts lvars (PlLit pn pargs)
  | withFunctions ts && withDemand ts && isUnif && isPlVar (head pargs) &&
    null (intersect lvars (termVars (head pargs)))
  -- handle X=t literals as binding X/t:
  = ([], [([head pargs], pargs!!1)])
  | withFunctions ts
  = if null res
      then ([PlStruct (toUnif pn) args], [])
      else if withDemand ts && all isPlVar res &&
              null (intersect lvars (termsVars res))
             then ([], [(res, call)])
             else ([PlStruct "=:=" [tupleTerm res, call]], [])
  | otherwise
  = ([PlStruct (toUnif pn) pargs], [])
 where
  (res,args) = partitionPredArguments (resultPos ts (pn, length pargs)) pargs
  call       = PlStruct pn args

  toUnif p = if p == "=" then "=:=" else p
  isUnif = pn == "=" && length pargs == 2

----------------------------------------------------------------------------
-- Auxiliaries for the transformation.

-- If a binding of a single variable is used only once in a list
-- of expressions, replace it in the expression and delete the binding.
substBindings :: [([PlTerm], PlTerm)] -> [([PlTerm], PlTerm)] -> [PlTerm]
              -> ([([PlTerm], PlTerm)], [PlTerm])
substBindings rembindings [] terms = (reverse rembindings, terms)
substBindings rembindings ((pts,bterm) : bindings) terms = case pts of
  [PlVar v] | numOccsOf v (bterm : map snd bindings ++ terms) <= 1 ->
               let subst = substTerm v bterm
               in substBindings (map (\ (p,bt) -> (p, subst bt)) rembindings)
                                (map (\ (p,bt) -> (p, subst bt)) bindings)
                                (map subst terms)
  _         -> substBindings ((pts,bterm) : rembindings) bindings terms
 where
  numOccsOf v es = length (filter (== v) (concatMap termVarOccs es))


-- Partitions a list of arguments of a predicate into the list of
-- result arguments (positions specified in the first argument)
-- and the remaining arguments.
partitionPredArguments :: [Int] -> [PlTerm] -> ([PlTerm], [PlTerm])
partitionPredArguments rpos allargs =
  let (nres,nargs) = partition ((`elem` rpos) . fst) (zip [1..] allargs)
  in (map snd nres, map snd nargs)

-- Constructs a tuple of terms.
tupleTerm :: [PlTerm] -> PlTerm
tupleTerm args
  | n == 0    = PlAtom "()"
  | n == 1    = head args
  | otherwise = PlStruct ('(' : take (n - 1) (repeat ',') ++ ")") args
 where
  n = length args

-- Translates a Prolog term into a Curry expression.
transTerm :: TransState -> PlTerm -> CExpr
transTerm ts pterm = case pterm of
  PlVar v       -> cvar (lowerFirst v)
  PlInt i       -> cInt i
  PlFloat i     -> cFloat i
  PlAtom a      -> constF (transName ts a)
  PlStruct s ps -> if s == "is"
                     then case length ps of -- remove "is" calls
                            1 -> transTerm ts (head ps)
                            _ -> transTerm ts (PlStruct "=:=" ps)
                     else applyF (transName ts s) (map (transTerm ts) ps)

-- Substitutes all occurrences of a variable in a Prolog term.
substTerm :: String -> PlTerm -> PlTerm -> PlTerm
substTerm sv sterm pterm = case pterm of
  PlVar v       -> if v == sv then sterm else pterm
  PlStruct s ps -> PlStruct s (map (substTerm sv sterm) ps)
  _             -> pterm

----------------------------------------------------------------------------
-- Auxiliaries:

plTrue :: PlTerm
plTrue = PlAtom "True"

-- Is a Prolog term a variable?
isPlVar :: PlTerm -> Bool
isPlVar pterm = case pterm of PlVar _ -> True
                              _       -> False

-- The name and arity of the root of a Prolog term.
rootOf :: PlTerm -> (String,Int)
rootOf pterm = case pterm of
  PlVar _         -> ("", 0)
  PlInt i         -> (show i, 0)
  PlFloat x       -> (show x, 0)
  PlAtom a        -> (a, 0)
  PlStruct s args -> (s, length args)

-- The arguments of a Prolog term.
argsOf :: PlTerm -> [PlTerm]
argsOf pterm = case pterm of
  PlStruct _ args -> args
  _               -> []

-- The set of all variables in a Prolog goal.
goalVars :: PlGoal -> [String]
goalVars pgoal = case pgoal of
  PlLit _ ts         -> termsVars ts
  PlNeg goals        -> unionMap goalVars goals
  PlCond gs1 gs2 gs3 -> unionMap (unionMap goalVars) [gs1,gs2,gs3]

-- The set of all variables in a Prolog term.
termVars :: PlTerm -> [String]
termVars pterm = case pterm of
  PlVar v       -> [v]
  PlStruct _ ts -> termsVars ts
  _             -> []

-- The set of all variables in a list of Prolog terms.
termsVars :: [PlTerm] -> [String]
termsVars = unionMap termVars

-- The multi-set of all occurrences of variables in a Prolog term.
termVarOccs :: PlTerm -> [String]
termVarOccs pterm = case pterm of
  PlVar v       -> [v]
  PlStruct _ ts -> concatMap termVars ts
  _             -> []

-- The set of all data constructors with their arities used in a Prolog goal.
goalConstrs :: PlGoal -> [(String,Int)]
goalConstrs pgoal = case pgoal of
  PlLit _ ts         -> unionMap termConstrs ts
  PlNeg goals        -> unionMap goalConstrs goals
  PlCond gs1 gs2 gs3 -> unionMap (unionMap goalConstrs) [gs1,gs2,gs3]

-- The set of all data constructors with their airities used in a Prolog term.
termConstrs :: PlTerm -> [(String,Int)]
termConstrs pterm = case pterm of
  PlAtom a      -> [(a,0)]
  PlStruct s ts -> union [(s, length ts)] (unionMap termConstrs ts)
  _             -> []

-- Translates a Prolog atom with a given arity into a qualified Curry name.
transName :: TransState -> String -> QName
transName ts s
  | s == "."  = if useLists ts then pre ":" else (mn, "CONS")
  | s == "[]" = if useLists ts then pre s   else (mn, "NIL")
  --| s == "="  = pre "=:="
  | s `elem` ["True", "False", "&&"] = pre s
  | s `elem` map fst stdNames
  = maybe (error "Internal error transName") pre (lookup s stdNames)
  | otherwise
  = (mn, maybe (upperFirst s)
               snd
               (find (\ ((p,_),_) -> p==s) (prologPreds ts)))
 where
  mn = modName ts

stdNames :: [(String,String)]
stdNames =
  [ ("=" , "==")
  , ("\\=", "/=")
  , ("=<", "<=")
  , (">=", ">=")
  , ("<" , "<" )
  , (">" , ">" )
  ]

-- Simple comparison predicates
simpleCmpPreds :: [String]
simpleCmpPreds = ["=","\\=","<",">","=<",">="]

-- if-then-else expression
cITE :: CExpr -> CExpr -> CExpr -> CExpr
cITE c t e = applyF (pre "if_then_else") [c,t,e]

----------------------------------------------------------------------------
unionMap :: Eq b => (a -> [b]) -> [a] -> [b]
unionMap f = foldr union [] . map f

-- Transform first character into uppercase.
upperFirst :: String -> String
upperFirst [] = []
upperFirst (c:cs) = toUpper c : cs

-- Transform first character into lowercase.
lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (c:cs) = toLower c : cs

----------------------------------------------------------------------------
