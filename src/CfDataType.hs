{-# OPTIONS -XTypeSynonymInstances #-}
module CfDataType where
import Control.Monad
import Data.List

---Top definition
data TopDefine = TopDefine [Definition]

data Definition = DefFun 
                | DefVar
                | DefConst
                | DefStruct
                | DefUnion
                | DefType
    
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

---Statement------------
data Statement = IfStatement { if_cont::Expression, if_then::Expression, if_else::Expression }
               | WhileStatement { while_cont::Expression, while_exps::[Expression] }
                 deriving Show

---Expression----------
data Expression = AssignExp { assign_name::Name, assign_value::Expression }
                | TermExp Term

class Expressions a where
    toExpression :: a -> Expression

instance Show Expression where
    show (AssignExp name value) = name ++ " = " ++ (show value)
    show (TermExp term) = show term
                          
---Term----------------
data Term = NumberTerm Number

class Terms a where
    toTerm :: a -> Term
          
instance Show Term where
    show (NumberTerm num) = show num
                            
instance Expressions Term where
    toExpression term = TermExp term

---Number--------------
type Number = Int

instance Terms Int where
    toTerm i = NumberTerm i
               
---Variable-----------
type Name = String               



