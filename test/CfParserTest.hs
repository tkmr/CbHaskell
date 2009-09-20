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
  expResult <- parserTest expressionParser expressionGenerator
  impResult <- parserTest impStatementsParser impStatementsGenerator
               
  notify $ ((isNotError impResult) && (isNotError expResult))

  
parserTest parser generator = do
  exp <- generateN 1 generator
  res <- return $ tryParse exp
  putStr $ (show res) ++ "\n"
  return res
    where
      tryParse x = parse parser "" $ show x

---Util
runruby message name = runInteractiveProcess "ruby" ["/home/tatsuya/study/ruby/notify_client.rb", "192.168.0.6", "7878", message, name] Nothing Nothing
    
notify True  = runruby "build is success" "success"
notify False = runruby "build is fail..." "fail"

isNotError (Left  _)  = False
isNotError (Right _)  = True
