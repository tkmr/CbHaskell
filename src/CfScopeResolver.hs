{-# OPTIONS -XPatternSignatures #-}
module CfScopeResolver where
import CfDataType
import CfDataType_Show
import CfCheckAccepter
import qualified CfEntity as Ent
import Data.Map as Map
import Data.Maybe
import Data.IORef

class CheckAccepter a  => CheckVisitor a where
    visit :: Checker -> a -> IO a
    visit checker target = accept target checker

data Checker = ScopeChecker { checker_scope::(IORef Scope) }
             | TypeChecker

instance CheckVisitor DefFun where
    visit (ScopeChecker scope) defn = do modifyIORef scope (putScopeBaseToStack $ fromParamsToStackBase $ deffun_params defn)
                                         accept defn (ScopeChecker scope)

instance CheckVisitor DefVar where
    visit (ScopeChecker scope) defv = do modifyIORef scope (addVarToScope defv)
                                         accept defv (ScopeChecker scope)

instance CheckVisitor Block where
    visit (ScopeChecker scope) block = do modifyIORef scope (putScopeBaseToStack $ ScopeBase [] Map.empty)
                                          accept block (ScopeChecker scope)

instance CheckVisitor Term where
    visit (ScopeChecker scope) (VarIdentLiteral name) = do realscope <- readIORef scope
                                                           return $ VariableTerm name (findEntity realscope name)
                                                                     
instance CheckVisitor FuncParams
instance CheckVisitor Param
instance CheckVisitor Statement
instance CheckVisitor Expression
instance CheckVisitor Type
instance CheckVisitor TyperefOption

-----------------------------------------------    
data Error = ScopeResolveError String

data Scope = TopScope   ScopeBase
           | LocalScope ScopeBase Scope

data ScopeBase = ScopeBase [Scope] (Map String Ent.Entity)

getScope :: Scope -> String -> Maybe Ent.Entity
getScope (TopScope   (ScopeBase _ map)       ) key = Map.lookup key map
getScope (LocalScope (ScopeBase _ map) parent) key = Map.lookup key map

putScope :: Scope -> String -> Ent.Entity -> Scope
putScope (TopScope   (ScopeBase c map)       ) key val = TopScope $ ScopeBase c $ Map.insert key val map
putScope (LocalScope (ScopeBase c map) parent) key val = LocalScope (ScopeBase c $ Map.insert key val map) parent
--putScope (LocalScope (ScopeBase c map) parent) key _  = error "LocalScope can accept only DefVarEntity"

findEntity :: Scope -> String -> Ent.Entity
findEntity (TopScope (ScopeBase _ map))          key = case Map.lookup key map of
                                                         Just entity -> entity
                                                         Nothing     -> Prelude.error ("Can't found variable which is '"++ key ++"' in current scope... ")
findEntity (LocalScope (ScopeBase _ map) parent) key = case Map.lookup key map of
                                                         Just entity -> entity
                                                         Nothing     -> findEntity parent key

tryPutEntity :: Scope -> String -> Ent.Entity -> Scope
tryPutEntity scope key value = case getScope scope key of
                                 Just e  -> Prelude.error ("Can't duplicate define a variable... " ++ key)
                                 Nothing -> putScope scope key value

addVarToScope :: DefVar -> Scope -> Scope
addVarToScope defvar scope = tryPutEntity scope (defvar_name defvar) (Ent.toEntity defvar)
                                            
addVarsToScope :: [DefVar] -> Scope -> Scope
addVarsToScope []     scope = scope
addVarsToScope (d:ds) scope = addVarsToScope ds (addVarToScope d scope)


putScopeToStack :: Scope -> Scope -> Scope
putScopeToStack (TopScope   (ScopeBase scopes m)  ) newScope = TopScope   (ScopeBase (scopes ++ [newScope]) m)
putScopeToStack (LocalScope (ScopeBase scopes m) p) newScope = LocalScope (ScopeBase (scopes ++ [newScope]) m) p

putScopeBaseToStack :: ScopeBase -> Scope -> Scope
putScopeBaseToStack base scope = orgScope
    where
      orgScope = putScopeToStack scope newScope
      newScope = LocalScope base orgScope

deffun_params (DefFun _ _ _ params _) = params
defvar_name   (DefVar _ _ name _)     = name

fromDefvarsToStackBase :: [DefVar] -> ScopeBase
fromDefvarsToStackBase ds = ScopeBase [] $ Map.fromList $ Prelude.map (\defvar -> ((defvar_name defvar), Ent.toEntity defvar)) ds
                                        
fromParamsToStackBase :: FuncParams -> ScopeBase
fromParamsToStackBase (VoidParams)            = ScopeBase [] Map.empty
fromParamsToStackBase (FixedParams params)    = ScopeBase [] $ Map.fromList $ Prelude.map paramToNamedEntity params
fromParamsToStackBase (VariableParams params) = ScopeBase [] $ Map.fromList $ Prelude.map paramToNamedEntity params

paramToNamedEntity :: Param -> (String, Ent.Entity)
paramToNamedEntity (Param typ name) = (name, Ent.toEntity $ DefVar (StaticProp False) typ name Nothing)
