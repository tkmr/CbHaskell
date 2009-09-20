{-# OPTIONS -XTypeSynonymInstances #-}
import CfParser
import CfDataType
import CfDataGenerator
import System.Process (runInteractiveProcess)
import Text.ParserCombinators.Parsec
import Test.QuickCheck
import System.Random
import Control.Monad

main = do
  exp       <- parserGenTest expressionParser (valid::(Gen Expression))
  imp       <- parserGenTest impStatementsParser (valid::(Gen ImportStatements))
  defvar    <- parserGenTest defvarsParser (valid::(Gen DefVar))
  deffun    <- parserGenTest deffunParser (valid::(Gen DefFun)) 
  defstruct <- parserTest defstructParser "struct hoge { int x; int y; }"
  defunion  <- parserTest defunionParser "union hoge { int x; int a; }"
  deftype   <- parserTest deftypeParser "typedef int NUMBER;"
               
  notify $ and [imp, exp, defvar, deffun, defstruct, defunion, deftype]

parserGenTest parser generator = do
  exp <- generateN 1 generator
  parserTest parser (show exp)
         
parserTest parser str = do
  putStr $ str ++ "\n"
  res <- return $ parse parser "" str
  putStr $ (show res) ++ "\n"
  return $ isNotError res

---Util
runruby message name = runInteractiveProcess "ruby" ["/home/tatsuya/study/ruby/notify_client.rb", "192.168.0.6", "7878", message, name] Nothing Nothing
    
notify True  = runruby "build is success" "success"
notify False = runruby "build is fail..." "fail"

isNotError (Left  _)  = False
isNotError (Right _)  = True
