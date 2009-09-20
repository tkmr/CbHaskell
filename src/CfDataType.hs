{-# OPTIONS -XTypeSynonymInstances #-}
module CfDataType where
import Control.Monad
import Data.List

---Top definition:
data TopDefine = TopDefine [Definition]

data Definition = DefineFun DefFun
                | DefineVar DefVar
                | DefConst  
                | DefStruct
                | DefUnion
                | DefType
                  deriving Show

---defvar:
data DefVar = DefVar StaticProp TypeName Name (Maybe Expression)
instance Show DefVar where
    show (DefVar isstatic typename name value) = (show isstatic) ++ typename ++ " " ++ name ++ (expOrEnd value)

---deffun:                                                 
data DefFun = DefFun Bool TypeName Name FuncParams Block
instance Show DefFun where
    show (DefFun isstatic typename name params body) = (show isstatic) ++ typename ++ " " ++ name ++ "(" ++ (show params) ++ ")" ++ show body
        
data FuncParams = VoidParams
                | FixedParams [Param]
                | VariableParams [Param]
instance Show FuncParams where
    show (VoidParams)            = "void"
    show (FixedParams params)    = joinStr ", " $ map show params
    show (VariableParams params) = joinStr ", " $ (map show params) ++ ["..."]

data Param = Param TypeName Name
instance Show Param where
    show (Param typename name) = typename ++ " " ++ name


---Sub definitions of Top define:    
data Block = Block [DefVar] [Statement]
instance Show Block where
    show (Block vars stmts) = (allShowNewline vars) ++ "\n\n" ++ (allShowNewline stmts)
    
    
---ImportStatement------
data ImportStatements = ImportStatements [ImportStatement]
data ImportStatement  = ImportStatement  [String]
                      
instance Show ImportStatements where
    show (ImportStatements [])     = ""
    show (ImportStatements (x:xs)) = (show x) ++ "\n" ++ (show xs)

instance Show ImportStatement where
    show (ImportStatement []) = ""
    show (ImportStatement xs) = "import " ++ (joinStr "." xs) ++ ";"

---Statement------------
data Statement = IfStatement { if_cont::Expression, if_then::Block, if_else::Block }
               | WhileStatement { while_cont::Expression, while_body::Block }
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

type TypeName = String


----misc-------------
data StaticProp = StaticProp Bool
    
instance Show StaticProp where
    show (StaticProp static) = case static of
                                 True -> "static "
                                 False -> ""
                  
expOrEnd (Nothing  ) = ";"
expOrEnd (Just  exp) = " = " ++ (show exp) ++ ";"

joinStr sep xs = foldl (++) "" $ intersperse sep xs

allShowNewline lists = joinStr "\n" $ map show lists