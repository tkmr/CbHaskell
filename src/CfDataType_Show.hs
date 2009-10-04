{-# OPTIONS -XTypeSynonymInstances #-}
module CfDataType_Show where
import CfDataType

instance Show ImportStatements where
    show (ImportStatements [])     = ""
    show (ImportStatements (x:xs)) = (show x) ++ "\n" ++ (show xs)

instance Show ImportStatement where
    show (ImportStatement []) = ""
    show (ImportStatement xs) = "import " ++ (joinStr "." xs) ++ ";"

instance Show DefVar where
    show (DefVar isstatic type_ name value) = (show isstatic) ++ (show type_) ++ " " ++ name ++ (expOrEnd value)

instance Show DefFun where
    show (DefFun isstatic type_ name params body) = (show isstatic) ++ (show type_) ++ " " ++ name ++ "(" ++ (show params) ++ ")" ++ show body

instance Show DefStruct where
    show (DefStruct name params) = "struct " ++ (show name) ++ "{\n" ++ (joinStr ";\n" $ map show params)  ++ ";\n}"

instance Show DefUnion where
    show (DefUnion name params) = "union " ++ (show name) ++ "{\n" ++ (joinStr ";\n" $ map show params)  ++ ";\n}"

instance Show DefType where
    show (DefType typ name) = "typedef " ++ (show typ) ++ " " ++ name ++ ";\n"
                                                    
instance Show FuncParams where
    show (VoidParams)            = "void"
    show (FixedParams params)    = joinStr ", " $ map show params
    show (VariableParams params) = joinStr ", " $ (map show params) ++ ["..."]

instance Show Param where
    show (Param type_ name) = (show type_) ++ " " ++ name

instance Show Block where
    show (Block vars stmts) = "{\n" ++ (allShowNewline vars) ++ "\n\n" ++ (allShowNewline stmts) ++ "\n}"

instance Show Statement where
    show (NulllineStatement)       = ";\n"
    show (LabeledStatement name)   = "label " ++ name ++ ":\n"
    show (IfStatement exp thn els) = "if (" ++ (show exp) ++ ") " ++ (show thn) ++ " else " ++ (show els) 
    show (BlockStatement block)    = show block
    show (ExpStatement exp)        = (show exp) ++ ";\n"
    show (WhileStatement exp body) = "while(" ++ (show exp) ++ ")" ++ (show body)
    show (ForStatement e1 e2 e3 body) = "for (" ++ (show e1) ++ ";" ++ (show e2) ++ ";" ++ (show e3) ++ ")" ++ (show body)
    show (BreakStatement)          = "break;\n"
    show (ContinueStatement)       = "continue;\n"
    show (GotoStatement name)      = "goto " ++ name ++ ";\n"
    show (ReturnStatement exp)     = "return " ++ (show exp) ++ ";\n"

instance Show Expression where
    show (AssignExp name ope value) = (show name) ++ " " ++ ope ++ " " ++ (show value)
    show (TermExp term)          = show term
    show (NullExp)               = "null"
    show (TreeConditionExp e1 e2 e3) = (show e1) ++ " ? " ++ (show e2) ++ " : " ++ (show e3)
    show (BoolCondExp e1 e2 op)  = (show e1) ++ op ++ (show e2)
    show (BitcalcExp e1 e2 op)   = (show e1) ++ op ++ (show e2)
    show (MathcalcExp e1 e2 op)  = (show e1) ++ op ++ (show e2)

instance Show Term where
    show (CastTerm tp tm)       = "(" ++ (show tp) ++ ")" ++ (show tm)
    show (PrefixCalcTerm pr tm) = pr ++ (show tm)
    show (TypesizeTerm tp)      = "sizeof (" ++ (show tp) ++ ")"
    show (SizeTerm tm)          = "sizeof " ++ (show tm)
    show (PostfixCalcTerm ps tm) = (show tm) ++ ps
    show (ArrayrefTerm exp tm)  = (show tm) ++ "[" ++ (show exp) ++ "]"
    show (StructrefTerm nm tm)  = (show tm) ++ "." ++ nm
    show (PointerrefTerm nm tm) = (show tm) ++ "->" ++ nm
    show (FunccallTerm exps tm) = (show tm) ++ "(" ++ (joinStr ", " $ map show exps) ++ ")"
    show (NumberLiteral num)    = show num
    show (CharLiteral chr)      = "'" ++ [chr] ++ "'"
    show (StringLiteral str)    = "\"" ++ str ++ "\""
    show (VarIdentLiteral name) = show name
    show (ExpLiteral exp)       = "(" ++ (show exp) ++ ")"

instance Show Type where
    show (Type base options) = (show base) ++ (joinStr "" $ map show options)

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

instance Show StaticProp where
    show (StaticProp static) = case static of
                                 True -> "static "
                                 False -> ""
