module CfDataType where
type TypeRef = Type
type Name = String

data DefVar = DefVar StaticProp TypeRef Name (Maybe Expression)

data StaticProp = StaticProp Bool

data Expression = AssignExp Term String Expression
                | TermExp Term
                | NullExp
                | TreeConditionExp Expression Expression Expression
                | BoolCondExp Expression Expression String
                | BitcalcExp  Expression Expression String
                | MathcalcExp Expression Expression String

data Term

data Type = Type TyperefBase [TyperefOption]

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

