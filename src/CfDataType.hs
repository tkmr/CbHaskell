module CfDataType where
import Control.Monad
import Data.List

---ImportStatement------
data ImportStatements = ImportStatements [ImportStatement]
data ImportStatement  = ImportStatement  [String]

---Top definition:
data TopDefine = TopDefine [Definition]

data Definition = DefineFun DefFun
                | DefineVar DefVar
                | DefConst  
                | DefineStruct DefStruct
                | DefineUnion  DefUnion
                | DefineType   DefType

data DefVar = DefVar StaticProp Type Name (Maybe Expression)
data DefFun = DefFun StaticProp TypeRef Name FuncParams Block
data DefStruct = DefStruct Name [Param]
data DefUnion = DefUnion Name [Param]
data DefType = DefType TypeRef Name

---Sub definitions of Top define:
data FuncParams = VoidParams
                | FixedParams [Param]
                | VariableParams [Param]

data Param = Param Type Name
data Block = Block [DefVar] [Statement]

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
                                     
---Expression----------
data Expression = AssignExp { assign_t::Term, assign_operation::String, assign_value::Expression }
                | TermExp Term
                | NullExp
                | TreeConditionExp Expression Expression Expression
                | BoolCondExp Expression Expression String
                | BitcalcExp  Expression Expression String
                | MathcalcExp Expression Expression String

---Term----------------
data Term = CastTerm        Type Term
          | PrefixCalcTerm  String Term
          | TypesizeTerm    Type
          | SizeTerm        Term
          | PostfixCalcTerm String Term
          | ArrayrefTerm    Expression Term
          | StructrefTerm   Name Term
          | PointerrefTerm  Name Term
          | FunccallTerm    [Expression] Term
          | NumberLiteral   Number
          | CharLiteral     Char
          | StringLiteral   String
          | VarIdentLiteral Name
          | ExpLiteral      Expression

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

                                     
----misc-------------
type Name = String
type Number = Int
data StaticProp = StaticProp Bool
    
expOrEnd (Nothing  ) = ";"
expOrEnd (Just  exp) = " = " ++ (show exp) ++ ";"
joinStr sep xs = foldl (++) "" $ intersperse sep xs
allShowNewline lists = joinStr "\n" $ map show lists