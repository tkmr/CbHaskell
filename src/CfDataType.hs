{-# OPTIONS -XTypeSynonymInstances #-}
module CfDataType where
import Test.QuickCheck
import System.Random
import Control.Monad
import Data.List

---ImportStatement------
data ImportStatements = ImportStatements [ImportStatement]
data ImportStatement  = ImportStatement  [String]
                      
instance Show ImportStatements where
    show (ImportStatements [])     = ""
    show (ImportStatements (x:xs)) = (show x) ++ "\n" ++ (show xs)

instance Show ImportStatement where
    show (ImportStatement []) = ""
    show (ImportStatement xs) = "import " ++ (join "." xs) ++ ";"
        where
          join sep xs = foldl (++) "" $ intersperse sep xs

instance Valid ImportStatements where
    valid = impStatementsGenerator

impStatementsGenerator :: Gen ImportStatements
impStatementsGenerator = do
  stA <- genImpStmt
  stB <- genImpStmt
  stC <- genImpStmt
  return $ ImportStatements [stA, stB, stC]

genImpStmt :: Gen ImportStatement
genImpStmt = do
  name <- rndStrGen "" 10
  return $ ImportStatement [name, name, name]
                        
---Statement------------
data Statement = IfStatement { if_cont::Expression, if_then::Expression, if_else::Expression }
               | WhileStatement { while_cont::Expression, while_exps::[Expression] }
                 deriving Show

---Expression----------
data Expression = AssignExp { assign_name::VariableName, assign_value::Expression }
                | TermExp Term

class Expressions a where
    toExpression :: a -> Expression

instance Show Expression where
    show (AssignExp name value) = name ++ " = " ++ (show value)
    show (TermExp term) = show term

instance Valid Expression where
    valid = oneof [ liftM2 AssignExp valid valid
                  , liftM TermExp valid
                  ]
                          
---Term----------------
data Term = NumberTerm Number

class Terms a where
    toTerm :: a -> Term
          
instance Show Term where
    show (NumberTerm num) = show num

instance Valid Term where
    valid = oneof [ liftM NumberTerm valid ]
                            
instance Expressions Term where
    toExpression term = TermExp term

---Number--------------
type Number = Int

instance Terms Int where
    toTerm i = NumberTerm i
               
---Variable-----------
type VariableName = String               

    
---for QuickCheck ---------------------------
generateN n generator = do
  rnd <- newStdGen
  return $ generate n rnd generator

expressionGenerator :: Gen Expression
expressionGenerator = do
  e <- valid
  return e

rndStrGen :: String -> Int -> Gen String
rndStrGen s 0 = return s
rndStrGen s n = do
  e <- elements $ map (:[]) ['a' .. 'z']
  rndStrGen (s ++ e) (n - 1)

rndIntGen :: Gen Int
rndIntGen = do
  e <- elements [0..1000]
  return e
            
class Valid a where
    valid :: Gen a

instance Valid String where
    valid = rndStrGen "" 10

instance Valid Int where
    valid = oneof [ rndIntGen ]            
