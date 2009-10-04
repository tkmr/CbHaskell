module CfScopeResolver where
import CfDataType
import Data.IORef
import Data.Map

data Scope = TopScope   ScopeBase
           | LocalScope ScopeBase Scope

data ScopeBase

data Checker

class CheckVisitor a

instance CheckVisitor Definition
instance CheckVisitor DefVar
instance CheckVisitor DefFun
instance CheckVisitor DefStruct
instance CheckVisitor DefUnion
instance CheckVisitor DefType
instance CheckVisitor FuncParams
instance CheckVisitor Param
instance CheckVisitor Block
instance CheckVisitor Statement
instance CheckVisitor Expression
instance CheckVisitor Term
instance CheckVisitor Type
instance CheckVisitor TyperefOption
instance CheckVisitor TyperefBase
