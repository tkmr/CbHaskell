{-# OPTIONS -XTypeSynonymInstances #-}
module CfDataGenerator where
import CfDataType    
import Test.QuickCheck
import System.Random
import Control.Monad
import Data.Maybe
import Data.List

class Valid a where
    valid :: Gen a

instance Valid ImportStatements where
    valid = liftM ImportStatements $ vectorOf 3 (valid::(Gen ImportStatement))

instance Valid ImportStatement where
    valid = liftM ImportStatement $ vectorOf 3 (valid::(Gen String))

instance Valid DefVar where
    valid = do name     <- valid
               typename <- valid
               exp      <- valid
               return $ DefVar (StaticProp True) typename name (Just exp)

instance Valid DefFun where
    valid = liftM5 DefFun valid valid valid valid valid

instance Valid FuncParams where
    valid = oneof [ elements [VoidParams]
                  , liftM FixedParams $ vectorOf 3 (valid::(Gen Param))
                  , liftM VariableParams $ vectorOf 3 (valid::(Gen Param))
                  ]

instance Valid StaticProp where
    valid = liftM StaticProp valid
            
instance Valid Param where
    valid = liftM2 Param valid valid

instance Valid Block where
    valid = liftM2 Block (vectorOf 2 (valid::(Gen DefVar))) (elements [ [(ExpStatement $ TermExp $ NumberTerm 10)] ]) --(listOf (valid::(Gen Statement)))

instance Valid Statement where
    valid = oneof [ liftM3 IfStatement valid valid valid ]
            
instance Valid Expression where
    valid = oneof [ liftM2 AssignExp valid valid
                  , liftM TermExp valid
                  ]

instance Valid Term where
    valid = liftM NumberTerm valid

instance Valid String where
    valid = rndStrGen "" 4

instance Valid Int where
    valid = oneof [ rndIntGen ]            

instance Valid Bool where
    valid = elements [ True, False]

            
---for QuickCheck ---------------------------
listOf :: Gen a -> Gen [a]
listOf gen = sized $ \n ->
             do k <- choose (0,n)
                vectorOf k gen

listOf1 :: Gen a -> Gen [a]
listOf1 gen = sized $ \n ->
              do k <- choose (1,1 `max` n)
                 vectorOf k gen

vectorOf :: Int -> Gen a -> Gen [a]
vectorOf k gen = sequence [ gen | _ <- [1..k] ]


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
            