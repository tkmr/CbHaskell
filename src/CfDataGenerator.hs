{-# OPTIONS -XTypeSynonymInstances #-}
module CfDataGenerator where
import CfDataType    
import Test.QuickCheck
import System.Random
import Control.Monad
import Data.List

---import
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

instance Valid ImportStatements where
    valid = impStatementsGenerator

--exp         
expressionGenerator :: Gen Expression
expressionGenerator = do
  e <- valid
  return e

instance Valid Expression where
    valid = oneof [ liftM2 AssignExp valid valid
                  , liftM TermExp valid
                  ]

--term
instance Valid Term where
    valid = oneof [ liftM NumberTerm valid ]

            
---for QuickCheck ---------------------------
generateN n generator = do
  rnd <- newStdGen
  return $ generate n rnd generator

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
    