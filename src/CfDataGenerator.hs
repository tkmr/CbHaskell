{-# OPTIONS -XTypeSynonymInstances #-}
module CfDataGenerator where
import CfDataType
import CfDataType_Show    
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
    valid = liftM2 Block (listOf (valid::(Gen DefVar))) (vectorOf 2 (valid::(Gen Statement)))

instance Valid Statement where
    valid = oneof [ liftM LabeledStatement valid
                  , liftM3 IfStatement valid valid valid
                  , liftM BlockStatement valid
                  , liftM ExpStatement valid
                  , liftM2 WhileStatement valid (liftM BlockStatement valid)
                  , liftM4 ForStatement valid valid valid valid
                  , elements[ BreakStatement, ContinueStatement ]
                  , liftM GotoStatement valid
                  , liftM ReturnStatement valid ]
            
instance Valid Expression where
    valid = frequency [ (2, (liftM3 AssignExp valid (elements ["=", "+=", "-="]) valid))
                      , (30, (liftM TermExp valid))
                      , (1, (liftM3 TreeConditionExp twoTermExpGen valid valid))
                      , (4, twoTermExpGen)
                      ]
        where
          twoTermExpGen  = oneof [ boolCondExpGen, bitExpGen, mathExpGen ]
          boolCondExpGen = oneof [ bitExpGen, mathExpGen, (liftM3 BoolCondExp valid valid (elements ["||", "&&", "<", "=="])) ]
          bitExpGen      = oneof [ mathExpGen, (liftM3 BitcalcExp valid valid (elements ["|", "&", "^", "<<"])) ]
          mathExpGen     = liftM3 MathcalcExp valid valid (elements ["+", "-", "&", "/"])

instance Valid Term where
    valid = frequency [ (30, (liftM  NumberLiteral valid))
                      , (5,  (liftM2 CastTerm valid valid))
                      , (2,  (liftM2 PrefixCalcTerm valid valid))
                      , (10, (liftM  TypesizeTerm valid))
                      , (10, (liftM  SizeTerm valid))
                      , (2,  (liftM2 PostfixCalcTerm valid valid))
                      , (5,  (liftM2 ArrayrefTerm valid valid))
                      , (5,  (liftM2 StructrefTerm valid valid))
                      , (5,  (liftM2 PointerrefTerm valid valid))
                      , (5,  (liftM2 FunccallTerm (vectorOf 2 (valid::(Gen Expression))) valid))
                      , (30, (liftM CharLiteral valid))
                      , (30, (liftM StringLiteral valid))
                      , (30, (liftM VarIdentLiteral valid))
                      , (1,  (liftM ExpLiteral valid))
                      ]

instance Valid Char where
    valid = elements ['a'..'z']
            
instance Valid String where
    valid = rndStrGen "" 4

instance Valid Int where
    valid = oneof [ rndIntGen ]            

instance Valid Bool where
    valid = elements [ True, False]

instance Valid TypeRef where
    valid = liftM2 Type valid $ listOf (valid::(Gen TyperefOption))

instance Valid TyperefBase where
    valid = elements [CharType, ShortType, IntType, LongType, UnsignedCharType, UnsignedShortType
                     , UnsignedIntType, UnsignedLongType, StructType "tsetstrct", UnionType "testunion"]

instance Valid TyperefOption where
    valid = oneof [ elements [NonLimitArrayOption, PointerOption]
                  , liftM LimitedArrayOption valid
                  , liftM FuncPointerOption $ listOf1 (valid::(Gen TypeRef))
                  ]
            
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
            