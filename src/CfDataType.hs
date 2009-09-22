{-# OPTIONS -XTypeSynonymInstances #-}
module CfDataType where
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
    show (ImportStatement xs) = "import " ++ (joinStr "." xs) ++ ";"

    
---Top definition:
data TopDefine = TopDefine [Definition]

data Definition = DefineFun DefFun
                | DefineVar DefVar
                | DefConst  
                | DefineStruct DefStruct
                | DefineUnion  DefUnion
                | DefineType   DefType
                  deriving Show

data DefVar = DefVar StaticProp Type Name (Maybe Expression)
            
instance Show DefVar where
    show (DefVar isstatic type_ name value) = (show isstatic) ++ (show type_) ++ " " ++ name ++ (expOrEnd value)

data DefFun = DefFun StaticProp TypeRef Name FuncParams Block
            
instance Show DefFun where
    show (DefFun isstatic type_ name params body) = (show isstatic) ++ (show type_) ++ " " ++ name ++ "(" ++ (show params) ++ ")" ++ show body
        
data DefStruct = DefStruct Name [Param] deriving Show

data DefUnion = DefUnion Name [Param] deriving Show

data DefType = DefType TypeRef Name deriving Show
                           

              
---Sub definitions of Top define:
data FuncParams = VoidParams
                | FixedParams [Param]
                | VariableParams [Param]
                  
instance Show FuncParams where
    show (VoidParams)            = "void"
    show (FixedParams params)    = joinStr ", " $ map show params
    show (VariableParams params) = joinStr ", " $ (map show params) ++ ["..."]

data Param = Param Type Name
           
instance Show Param where
    show (Param type_ name) = (show type_) ++ " " ++ name

data Block = Block [DefVar] [Statement]
           
instance Show Block where
    show (Block vars stmts) = "{\n" ++ (allShowNewline vars) ++ "\n\n" ++ (allShowNewline stmts) ++ "\n}"
    

---Statement------------
data Statement = NulllineStatement
               | LabeledStatement Name
               | ExpStatement   Expression     
               | BlockStatement Block
               | IfStatement    Expression Statement Statement
               | WhileStatement Expression Statement
               | ForStatement   Expression Expression Expression Statement
               | BreakStatement
               | ContinueStatement
               | GotoStatement  Name
               | ReturnStatement Expression

instance Show Statement where
    show (NulllineStatement)       = ";\n"
    show (LabeledStatement name)   = "label " ++ name ++ ":\n"
    show (IfStatement exp thn els) = "if (" ++ (show exp) ++ ") " ++ (show thn) ++ " else " ++ (show els) 
    show (BlockStatement block)    = show block
    show (ExpStatement exp)        = (show exp) ++ ";\n"
    show (WhileStatement exp body) = "while(" ++ (show exp) ++ ")" ++ (show body)
    show (ForStatement e1 e2 e3 body) = "for(" ++ (show e1) ++ ";" ++ (show e2) ++ ";" ++ (show e3) ++ ")" ++ (show body)
    show (BreakStatement)          = "break;\n"
    show (ContinueStatement)       = "continue;\n"
    show (GotoStatement name)      = "goto " ++ name ++ ";\n"
    show (ReturnStatement exp)     = "return " ++ (show exp) ++ ";\n"
                                     
---Expression----------
data Expression = AssignExp { assign_name::Name, assign_value::Expression }
                | TermExp Term
                | NullExp

instance Show Expression where
    show (AssignExp name value) = name ++ " = " ++ (show value)
    show (TermExp term) = show term
    show (NullExp)      = "null"

                         
---Term----------------
data Term = NumberTerm Number
          
instance Show Term where
    show (NumberTerm num) = show num


---Type--------------------
type Type = TypeRef

data TypeRef = TypeRef TyperefBase [TyperefOption]

data TyperefOption = NonLimitArrayOption
                   | LimitedArrayOption Int
                   | PointerOption
                   | FuncPointerOption [Type]

data TyperefBase = VoidType
                 | CharType
                 | ShortType
                 | IntType
                 | LongType
                 | UnsignedCharType
                 | UnsignedShortType
                 | UnsignedIntType
                 | UnsignedLongType
                 | StructType Name
                 | UnionType Name
                 | OriginalType Name

instance Show TypeRef where
    show (TypeRef base options) = (show base) ++ (joinStr "" $ map show options)

instance Show TyperefOption where
    show (NonLimitArrayOption)  = "[]"
    show (LimitedArrayOption i) = "[" ++ (show i) ++ "]"
    show (PointerOption)        = "*"
    show (FuncPointerOption types) = "(" ++ (joinStr "," $ map show types) ++ ")"

instance Show TyperefBase where
    show (VoidType)           = "void"
    show (CharType)           = "char"
    show (ShortType)          = "short"
    show (IntType)            = "int"
    show (LongType)           = "long"
    show (UnsignedCharType)   = "unsigned char"
    show (UnsignedShortType)  = "unsigned short"
    show (UnsignedIntType)    = "unsigned int"
    show (UnsignedLongType)   = "unsigned long"
    show (StructType name)    = "struct " ++ name
    show (UnionType  name)    = "union " ++ name
    show (OriginalType name)  = name
                                     
---Number--------------
type Number = Int


               
---Variable-----------
type Name = String               

    
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