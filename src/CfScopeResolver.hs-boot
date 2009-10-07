module CfScopeResolver where
import CfDataType
import Data.IORef
import Data.Map
import CfEntity as Ent

data Checker = ScopeChecker { checker_scope::(IORef Scope) }
             | TypeChecker

data Error = ScopeResolveError String

data Scope = TopScope   ScopeBase
           | LocalScope ScopeBase Scope

data ScopeBase = ScopeBase [Scope] (Map String Ent.Entity)


putScopeToStack :: Scope -> Scope -> Scope
putScopeBaseToStack :: ScopeBase -> Scope -> Scope
fromDefvarsToStackBase :: [DefVar] -> ScopeBase
fromParamsToStackBase :: FuncParams -> ScopeBase
paramToNamedEntity :: Param -> (String, Ent.Entity)
deffun_params :: DefFun -> FuncParams
addVarToScope :: DefVar -> Scope -> Scope
addVarsToScope :: [DefVar] -> Scope -> Scope
findEntity :: Scope -> String -> Ent.Entity

--instance CheckVisitor Definition
--instance CheckVisitor DefVar
--instance CheckVisitor DefFun
--instance CheckVisitor DefStruct
--instance CheckVisitor DefUnion
--instance CheckVisitor DefType
--instance CheckVisitor FuncParams
--instance CheckVisitor Param
--instance CheckVisitor Block
--instance CheckVisitor Statement
--instance CheckVisitor Expression
--instance CheckVisitor Term
--instance CheckVisitor Type
--instance CheckVisitor TyperefOption
--instance CheckVisitor TyperefBase
