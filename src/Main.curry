------------------------------------------------------------------------------
--- Main module to call the prolog2curry transformation tool.
---
--- @author Michael Hanus
--- @version January 2022
------------------------------------------------------------------------------

module Main
 where

import Curry.Compiler.Distribution ( installDir )
import Control.Monad               ( when, unless )
import Data.List                   ( isSuffixOf )
import Numeric                     ( readNat )
import System.Console.GetOpt
import System.Environment          ( getArgs )

import System.FilePath             ( (</>), dropExtension )
import System.Process              ( exitWith, system )

import AbstractCurry.Pretty        ( showCProg )
import Language.Prolog.Read        ( readPrologFile )
import Language.Prolog.Show        ( showPlClause, showPlProg )
import Language.Prolog.ToCurry

------------------------------------------------------------------------------
toolBanner :: String
toolBanner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "Prolog->Curry transformation tool (Version of 06/01/22)"
  bannerLine = take (length bannerText) (repeat '=')

main :: IO ()
main = do
  args <- getArgs
  (opts,progs) <- processOptions toolBanner args
  case progs of
    []  -> error "Prolog program name missing"
    [p] -> transformProgram opts p
    _   -> error "Too many program names provided"

-- Reads Prolog program from a file (with suffix `.pl`)
-- and print the transformed program.
transformProgram :: TransState -> String -> IO ()
transformProgram ts pname = do
  let progname = dropExtension pname
  pp <- readPrologFile (progname ++ ".pl")
  when (optVerb ts > 2) $ putStrLn $ encloseInLines $
    "Prolog program '" ++ pname ++ "':\n\n" ++ showPlProg pp
  let (cprog,ts1) = prolog2Curry (setModName progname ts) pp
      ucprog   = unlines (filter (not . (":: ()" `isSuffixOf`))
                                 (lines (showCProg cprog)))
      outfile  = case optOutput ts of "-" -> ""
                                      ""  -> modName ts1 ++ ".curry"
                                      f   -> f
  when (optVerb ts > 0 && not (null (ignoredCls ts1))) $ putStrLn $
    "The following queries/directives are ignored:\n" ++
    unlines (map showPlClause (ignoredCls ts1))
  when (optVerb ts > 1 && useAnalysis ts) $ putStrLn $
    "Unique and demanded arguments of predicates:\n" ++ showUniqueArgs ts1
  when (optVerb ts > 1) $ putStrLn $
    "Function information used in the transformation:\n" ++ showResultArgs ts1
  when (optVerb ts > 1 || optOutput ts == "-") $ putStrLn $ encloseInLines $
    "Generated Curry module:\n\n" ++ ucprog
  unless (null outfile) $ do
    writeFile outfile $
      (if optNoWarn ts then noWarnings else missSigOpt) ++ ucprog
    putStrLn $ "Generated Curry module written into '" ++ outfile ++ "'"
  when (optLoad ts && null (optOutput ts)) $ do
    let cmd = installDir </> "bin" </> "pakcs --nocypm :load " ++ modName ts1
    when (optVerb ts > 1) $ putStrLn $ "Executing: " ++ cmd
    ec <- system cmd
    exitWith ec
 where
  hline = take 78 (repeat '-')

  encloseInLines s = unlines [hline, s, hline]

  missSigOpt = "{-# OPTIONS_FRONTEND -Wno-missing-signatures #-}\n\n"
  noWarnings = "{-# OPTIONS_FRONTEND -Wnone #-}\n\n"

------------------------------------------------------------------------------
--- Process the actual command line argument and return the options
--- and the name of the main program.
processOptions :: String -> [String] -> IO (TransState,[String])
processOptions banner argv = do
  let (funopts, args, opterrors) = getOpt Permute options argv
      opts = foldl (flip id) (initState "") funopts
  unless (null opterrors)
         (putStr (unlines opterrors) >> printUsage >> exitWith 1)
  when (optHelp opts) (printUsage >> exitWith 0)
  return (opts, args)
 where
  printUsage = putStrLn (banner ++ "\n" ++ usageText)

-- Help text
usageText :: String
usageText =
  usageInfo ("Usage: pl2curry [options] <Prolog program name>\n") options

-- Definition of actual command line options.
options :: [OptDescr (TransState -> TransState)]
options =
  [ Option "h?" ["help"]
           (NoArg (\opts -> opts { optHelp = True }))
           "print help and exit"
  , Option "q" ["quiet"]
           (NoArg (\opts -> opts { optVerb = 0 }))
           "run quietly (no output, only exit code)"
  , Option "v" ["verbosity"]
            (OptArg (maybe (checkVerb 2) (safeReadNat checkVerb)) "<n>")
            "verbosity level:\n0: quiet (same as `-q')\n1: show status messages (default)\n2: show generated program (same as `-v')\n3: show all details"
  , Option "o" ["output"]
           (ReqArg (\s opts -> opts { optOutput = s }) "<f>")
           ("output file for Curry program (or '-')\n(otherwise: store in PROG.curry)")
  , Option "r" ["run"]
           (NoArg (\opts -> opts { optLoad = True }))
           "load the Curry program after generating it"
  , Option "w" ["nowarn"]
           (NoArg (\opts -> opts { optNoWarn = True }))
           "turn off all warnings for generated Curry program"
  , Option "c" ["conservative"]
           (NoArg (\opts -> opts { withFunctions = False }))
           "conservative transformation into predicates"
  , Option "" ["nodemand"]
           (NoArg (\opts -> opts { withDemand = False }))
           "do not exploit demand evaluation via let bindings"
  , Option "" ["noinline"]
           (NoArg (\opts -> opts { withInline = False }))
           "do not inline where/let bindings in Curry code"
  , Option "" ["noanalysis"]
           (NoArg (\opts -> opts { useAnalysis = False }))
           "do not derive function information automatically"
  , Option "" ["anyresult"]
           (NoArg (\opts -> opts { optAnyResult = True }))
           "allow any position as result (not only the last)"
  , Option "" ["nolists"]
           (NoArg (\opts -> opts { useLists = False }))
           "do not use Curry lists but untyped raw lists"
  ]
 where
  safeReadNat opttrans s opts = case readNat s of
    [(n,"")] -> opttrans n opts
    _        -> error "Illegal number argument (try `-h' for help)"

  checkVerb n opts = if n>=0 && n<4
                       then opts { optVerb = n }
                       else error "Illegal verbosity level (try `-h' for help)"

------------------------------------------------------------------------------
