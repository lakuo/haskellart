-- | This module has various "utilities" that you can use to build a REPL. 

module Language.Nano.Repl where

import           Control.Exception
import           System.Exit
import           System.IO
import qualified Data.List as L
import qualified Data.Char as Char
import           Language.Nano.Types 
import           Language.Nano.Eval  

--------------------------------------------------------------------------------
welcome :: String
--------------------------------------------------------------------------------
welcome = unlines
  [ "------------------------------------------------------------"
  , "-------- The NANO Interpreter v.0.0.0.0 --------------------"
  , "------------------------------------------------------------"
  ]

--------------------------------------------------------------------------------
putStrFlush :: String -> IO ()
--------------------------------------------------------------------------------
putStrFlush str = do 
  putStr str
  hFlush stdout

--------------------------------------------------------------------------------
doQuit :: IO a 
--------------------------------------------------------------------------------
doQuit = do 
  putStrLn "Goodbye." 
  exitWith ExitSuccess 

--------------------------------------------------------------------------------
doEval :: Env -> String -> IO ()
--------------------------------------------------------------------------------
doEval env s = (print =<< execEnvString env s) `catch` (putStrLn . errMsg)

--------------------------------------------------------------------------------
doUnknown :: IO () 
--------------------------------------------------------------------------------
doUnknown = putStrLn "I'm sorry Dave, I'm sorry I can't do that..."

--------------------------------------------------------------------------------
doRun :: FilePath -> IO ()
--------------------------------------------------------------------------------
doRun f = (print =<< execFile f) `catch` (putStrLn . errMsg)

--------------------------------------------------------------------------------
doLoad :: FilePath -> IO Env
--------------------------------------------------------------------------------
doLoad f = (defsEnv =<< defsFile f) `catch` exitEnv

exitEnv :: Error -> IO Env
exitEnv err = putStrLn (errMsg err) >> return prelude 


--------------------------------------------------------------------------------
-- HINT: You may want to implement `defsEnv` and then use `doLoad`
--------------------------------------------------------------------------------
defsEnv :: [(Id, Expr)] -> IO Env
--------------------------------------------------------------------------------
-- [(id, exp)] -> [(id, value)]
-- return -> IO Env
newEnv :: [(Id, Expr)] -> Env
newEnv [] = []
newEnv ((id, exp):rest) = (id, eval [] exp) : newEnv rest
defsEnv xes =   return (prelude ++ newEnv xes)

--------------------------------------------------------------------------------
-- | A Datatype Commands for the shell -----------------------------------------
--------------------------------------------------------------------------------

data Cmd 
  = CEval String    -- ^ `CEval s` means parse-and-evaluate the `s`
  | CRun  FilePath  -- ^ `CRun f`  means parse-and-evaluate the "top" binder of `f`
  | CLoad FilePath  -- ^ `CLoad f` means parse-and-add-to-env all the binders of `f`
  | CQuit           -- ^ `CQuit`   means exit the shell
  | CUnknown        -- ^ any other unknown command
  deriving (Show)

strCmd :: String -> Cmd
strCmd str =  
	if L.isPrefixOf (":" ++ pfxQuit) str then CQuit
	else if L.isPrefixOf (":" ++ pfxRun) str then CRun (chomp 5 str)
	else if L.isPrefixOf (":" ++ pfxLoad) str then CLoad (chomp 6 str)
	else if L.isPrefixOf (":") str then CUnknown
	else CEval str

-- HINT: You may want to use the below functions and `L.isPrefixOf`, `chomp`, `pfxRun`, 

chomp :: Int -> String -> String
chomp n s = dropWhile Char.isSpace (drop n s)

pfxRun, pfxLoad, pfxQuit :: String 
pfxRun  = "run"
pfxLoad = "load"
pfxQuit = "quit"


