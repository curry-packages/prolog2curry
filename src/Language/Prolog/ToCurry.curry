------------------------------------------------------------------------------
--- This library contains operations to read/parse Prolog programs.
---
--- @author Michael Hanus
--- @version January 2022
------------------------------------------------------------------------------

module Language.Prolog.ToCurry
  ( TransState(..), initState, setModName, showUniqueArgs, showResultArgs
  , prolog2Curry )
 where

import Data.Char ( toLower, toUpper )
import Data.List ( (\\), intersect, isSuffixOf, last, maximum
                 , partition, transpose, union )
import System.IO.Unsafe ( trace )

import AbstractCurry.Build
import AbstractCurry.Pretty  ( showCProg )
import AbstractCurry.Select  ( varsOfExp )
import AbstractCurry.Types

import Language.Prolog.Read  ( readPrologFile )
import Language.Prolog.Show  ( showPlClause, showPlGoal, showPlProg )
import Language.Prolog.Types

-- Some testing:

m1 :: IO ()
m1 = transProg "rev"

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
  , prologPreds   :: [(PredSpec,String)] -- predicate spec and output name
  , prologCons    :: [(String,Int)]      -- structure name / arity
  , uniqueArgs    :: [(PredSpec,[Int])]  -- unique argument positions
  , resultArgs    :: [(PredSpec,[Int])]  -- result argument positions
  }

--- Returns an initial transformation state for a given module name.
initState :: String -> TransState
initState mname =
  TransState mname 1 False "" False True True False True True True
             [] [] [] initResultArgs
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

-- Looks up the unique argument positions for a predicate
-- in a transformation state.
uniquePos :: TransState -> PredSpec -> [Int]
uniquePos ts pnar = maybe [] id (lookup pnar (uniqueArgs ts))

-- Looks up result arguments for a predicate in a transformation state.
resultPos :: TransState -> PredSpec -> [Int]
resultPos ts pnar = maybe [] id (lookup pnar (resultArgs ts))

showPredPositions :: [(PredSpec,[Int])] -> String
showPredPositions = unlines .
  map (\ (pnar,pos) -> showPredArity pnar ++ ": " ++ unwords (map show pos))

showUniqueArgs :: TransState -> String
showUniqueArgs ts = showPredPositions (uniqueArgs ts)

showResultArgs :: TransState -> String
showResultArgs ts = showPredPositions (resultArgs ts)

showPredArity :: PredSpec -> String
showPredArity (pn,ar) = pn ++ "/" ++ show ar

----------------------------------------------------------------------------

--- Translates a list of Prolog clauses into an AbstractCurry program
--- and return also the modified translation state.
prolog2Curry :: TransState -> [PlClause] -> (CurryProg, TransState)
prolog2Curry ts cls =
  let (functiondirs, cls1) = extractFunctonDirectives cls
      predclauses = sortPredicates cls1
      allconstrsP = unionMap patsrhsconstrs (concatMap snd predclauses) \\
                      stdConstrs
      allconstrs = if useLists ts then allconstrsP \\ [("[]",0), (".",2)]
                                  else allconstrsP
      ts1 = ts { prologPreds = map (\ ((pn,ar),_) -> ((pn,ar),pn)) predclauses
               , prologCons  = allconstrs
               , resultArgs  = resultArgs ts ++ functiondirs }
      ts2 = if useAnalysis ts then analyzeClauses predclauses ts1
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
analyzeClauses cls ts = analyzeFunctions cls (analyzeUniqueArgs cls ts)

-- Derive `function` directives for all predicates defined in the given
-- list of clauses. Already existing directives are not changed.
analyzeFunctions :: [(PredSpec, [Clause])] -> TransState -> TransState
analyzeFunctions []                                  ts = ts
analyzeFunctions ((pnar@(pn,ar),pcls) : predclauses) ts =
  maybe (let fspecs = resultArgs ts
             ps     = computeResults fspecs pnar pcls
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
  computeResults funcspecs (pn,ar) cls
    | ar == 0
    = []   -- no result position
    | length cls == 1 -- defined by single rule:
      -- if the last argument is a non-variable or a variable defined in
      -- a result position, we interpret this predicate as a function:
    = case last (fst (head cls)) of
        PlVar v -> if isResultVar funcspecs v (snd (head cls)) then [ar]
                                                               else []
        _       -> [ar]
    | not (null unipos) -- defined by non-overlapping patterns
    = if optAnyResult ts
        then if ar == 1 then [] -- or non-deterministic operation?
                        else [maximum ([1 .. ar] \\ [head unipos])]
        else if null (unipos \\ [ar]) then [] else [ar]
    | otherwise
    = []
   where
    unipos = uniquePos ts (pn,ar)

-- Analyze the unique argument positions (i.e., demanded and pairwise disjoint)
-- for all predicates defined in the given list of clauses and add the
-- analysis results to the state.
analyzeUniqueArgs :: [(PredSpec, [Clause])] -> TransState -> TransState
analyzeUniqueArgs []                          ts = ts
analyzeUniqueArgs ((pnar,pcls) : predclauses) ts =
  maybe (let ps  = computeUniqueArgs pcls
             ts1 = if null ps
                     then ts
                     else ts { uniqueArgs = uniqueArgs ts ++ [(pnar,ps)] }
         in analyzeUniqueArgs predclauses ts1)
        (const $ analyzeUniqueArgs predclauses ts) -- keep existing uniqueArgs
        (lookup pnar (uniqueArgs ts))
 where
  computeUniqueArgs cls =
    map fst (filter (nonOverlappingConsTerms . snd)
                    (zip [1 ..] (transpose (map fst cls))))

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
                      in lvar `elem` unionMap termVars res

----------------------------------------------------------------------------
-- Translates a list of constructors into a data declaration.
constrs2type :: TransState -> [(String,Int)] -> CTypeDecl
constrs2type ts cs =
  CType termType Public [] (map c2cdecl cs) (map pre ["Eq", "Show"])
 where
  termType = (modName ts, "Term")

  c2cdecl (c,i) =
    CCons (transName ts c i) Public (map (const (baseType termType)) [1 .. i])

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
-- for each predicate. Ignores all directives and queries.
sortPredicates :: [PlClause] -> [(PredSpec, [Clause])]
sortPredicates []       = []
sortPredicates (cl:cls) = case cl of
  PlDirective _ -> trace ("Ignoring directive: " ++ showPlClause cl)
                         (sortPredicates cls)
  PlQuery     _ -> trace ("Ignoring query: " ++ showPlClause cl)
                         (sortPredicates cls)
  PlClause pn args goals ->
    let ar = length args
        (pnclauses,otherclauses) = partition (hasNameArity pn ar) cls
    in ((pn,ar), (args,goals) : map patsRhs pnclauses) :
       sortPredicates otherclauses
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
  cfunc (transName ts pn ar) ar Public
        (emptyClassType unitType) -- dummy type, will be removed
        (map (transClause ts (pn,ar)) clauses)

-- Translate a Prolog clause into a Curry rule.
transClause :: TransState -> PredSpec -> Clause -> CRule
transClause ts (pn,ar) (args, goals)
  | withFunctions ts = trClauseFunctional   ts (pn,ar) args goals
  | otherwise        = trClauseConservative ts args goals

-- Translate a Prolog clause into a Curry rule with the functional
-- transformation, i.e., consider the information about result positions.
trClauseFunctional :: TransState -> PredSpec -> [PlTerm] -> [PlGoal]
                   -> CRule
trClauseFunctional ts pnar predargs goals =
  let rpos = resultPos ts pnar
      (res,args) = partitionPredArguments rpos predargs
      rhs0      = if null res then PlAtom "True"
                              else tupleTerm res
      (guardexps,binds0) = unzip (map (transGoal ts (unionMap termVars args))
                                      goals)
      guard0    = if null (concat guardexps)
                    then PlAtom "True"
                    else foldr1 (\t1 t2 -> PlStruct "&&" [t1,t2])
                                (concat guardexps)
      (bindings, [guard, rhs]) =
         if withInline ts then substBindings [] (concat binds0) [guard0, rhs0]
                          else (concat binds0, [guard0, rhs0])
      extravars = filter (/="_")
                    (union (unionMap goalVars goals) (unionMap termVars res)
                       \\ unionMap termVars
                                   (args ++ concatMap fst (concat binds0)))
  in guardedRule (map (transPattern ts) args)
                 [(transTerm ts guard, transTerm ts rhs)]
       (map (\ (pts,e) -> CLocalPat (tuplePattern (map (transPattern ts) pts))
                                    (CSimpleRhs (transTerm ts e) []))
            bindings ++
        if null extravars
          then []
          else [CLocalVars (map (\v -> (1, lowerFirst v)) extravars)])

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


-- Translates a Prolog clause into a Curry rule with the convervative
-- transformation scheme, i.e., translates a Prolog predicate into a
-- Curry predicate.
trClauseConservative :: TransState -> [PlTerm] -> [PlGoal] -> CRule
trClauseConservative ts args goals =
  let extravars = filter (/="_")
                         (unionMap goalVars goals \\ unionMap termVars args)
      patterns  = map (transPattern ts) args
      rhs       = if null goals
                    then constF (pre "True")
                    else foldr1 (\t1 t2 -> applyF (pre "&&") [t1,t2])
                                (map (transTerm ts)
                                     (concatMap (fst . transGoal ts []) goals))
  in if null extravars
       then simpleRule patterns rhs
       else simpleRuleWithLocals patterns rhs
              [CLocalVars (map (\v -> (1, lowerFirst v)) extravars)]

-- Translates a Prolog term into a Curry pattern.
transPattern :: TransState -> PlTerm -> CPattern
transPattern ts pterm = case pterm of
  PlVar v       -> cpvar (lowerFirst v)
  PlInt i       -> pInt i
  PlFloat i     -> pFloat i
  PlAtom a      -> CPComb (transName ts a 0) []
  PlStruct s ps -> CPComb (transName ts s (length ps))
                          (map (transPattern ts) ps)

-- Translates a Prolog goal into an expression (representing the goal
-- as a Boolean condition) with local bindings in case of the demand
-- transformation. The second argument contains the lhs variables.
transGoal :: TransState -> [String] -> PlGoal
          -> ([PlTerm], [([PlTerm], PlTerm)])
transGoal ts lvars goal = case goal of
  PlNeg  _       -> error $ "Cannot translate negation: " ++ showPlGoal goal
  PlCond _ _ _   -> error $ "Cannot translate conditional: " ++ showPlGoal goal
  PlLit  pn pargs ->
    if withFunctions ts
      then let rpos = resultPos ts (pn, length pargs)
               (res,args) = partitionPredArguments rpos pargs
               call = PlStruct pn args
           in if null res
                then ([call], [])
                else if withDemand ts &&
                        null (intersect lvars (unionMap termVars res))
                       then ([], [(res, call)])
                       else ([PlStruct "=" [tupleTerm res, call]],
                             [])
      else ([PlStruct pn pargs], [])

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
  PlAtom a      -> constF (transName ts a 0)
  PlStruct s ps -> if s == "is"
                     then case length ps of -- remove "is" calls
                            1 -> transTerm ts (head ps)
                            _ -> transTerm ts (PlStruct "=" ps)
                     else applyF (transName ts s (length ps))
                                 (map (transTerm ts) ps)

-- Substitutes all occurrences of a variable in a Prolog term.
substTerm :: String -> PlTerm -> PlTerm -> PlTerm
substTerm sv sterm pterm = case pterm of
  PlVar v       -> if v == sv then sterm else pterm
  PlStruct s ps -> PlStruct s (map (substTerm sv sterm) ps)
  _             -> pterm

----------------------------------------------------------------------------
-- Auxiliaries:

-- Is a Prolog term a variable?
isPlVar :: PlTerm -> Bool
isPlVar pterm = case pterm of PlVar _ -> True
                              _       -> False

-- The set of all variables in a Prolog goal.
goalVars :: PlGoal -> [String]
goalVars pgoal = case pgoal of
  PlLit _ ts         -> unionMap termVars ts
  PlNeg goals        -> unionMap goalVars goals
  PlCond gs1 gs2 gs3 -> unionMap (unionMap goalVars) [gs1,gs2,gs3]

-- The set of all variables in a Prolog term.
termVars :: PlTerm -> [String]
termVars pterm = case pterm of
  PlVar v       -> [v]
  PlStruct _ ts -> unionMap termVars ts
  _             -> []

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
transName :: TransState -> String -> Int -> QName
transName ts s ar
  | s == "."  = if useLists ts then pre ":" else (mn, "CONS")
  | s == "[]" = if useLists ts then pre s   else (mn, "NIL")
  | s == "="  = pre "=:="
  | s `elem` ["True", "False", "&&"] = pre s
  | s `elem` map fst stdNames
  = maybe (error "Internal error transName") pre (lookup s stdNames)
  | otherwise
  --= (mn, if s `elem` map fst (prologPreds ts) then s else upperFirst s)
  = (mn, maybe (upperFirst s) id (lookup (s,ar) (prologPreds ts)))
 where
  mn = modName ts

stdNames :: [(String,String)]
stdNames =
  [ ("=", "=:=")
  , ("=<", "<=")
  , (">=", ">=")
  , ("<",  "<" )
  , (">",  ">" )
  ]

----------------------------------------------------------------------------
unionMap :: Eq b => (a -> [b]) -> [a] -> [b]
unionMap f = foldr union [] . map f

upperFirst :: String -> String
upperFirst [] = []
upperFirst (c:cs) = toUpper c : cs

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (c:cs) = toLower c : cs

----------------------------------------------------------------------------
