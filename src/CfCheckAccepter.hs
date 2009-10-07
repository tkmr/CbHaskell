
module CfCheckAccepter where
import {-# SOURCE #-} CfScopeResolver
import CfDataType
import Data.Maybe
import Data.IORef
import Data.Map as Map

try f (Left l)  = Left l
try f (Right r) = f r

tryMaybe :: (a -> IO a) -> Maybe a -> IO (Maybe a)
tryMaybe f (Nothing)  = do return Nothing
tryMaybe f (Just val) = do newval <- f val
                           return $ Just val

tryAll :: (a -> IO a) -> [a] -> IO [a]
tryAll f []     = do return []
tryAll f (x:xs) = do newx <- f x
                     remain <- tryAll f xs
                     return (newx:remain)

--------------------------------------------------------------------
class CheckAccepter a  => CheckVisitor a where
    visit :: Checker -> a -> IO a
    visit checker target = accept target checker

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

instance CheckVisitor DefType
instance CheckVisitor DefUnion
instance CheckVisitor DefStruct
instance CheckVisitor FuncParams
instance CheckVisitor Param
instance CheckVisitor Statement
instance CheckVisitor Expression
instance CheckVisitor Type
instance CheckVisitor TyperefOption

--------------------------------------------------------------------                            
class CheckAccepter a where
    accept :: a -> Checker -> IO a

instance CheckAccepter Definition where
    accept (DefineVar defvar) checker = do defvar_  <- visit checker defvar
                                           return $ DefineVar defvar_

    accept (DefineFun deffun) checker = do deffun_ <- visit checker deffun
                                           return $ DefineFun deffun_

    accept (DefineStruct defstruct) checker = do defst_ <- visit checker defstruct
                                                 return $ DefineStruct defst_

    accept (DefineUnion defunion) checker = do defuni_ <- visit checker defunion
                                               return $ DefineUnion defuni_

    accept (DefineType deftype) checker = do deftp_ <- visit checker deftype
                                             return $ DefineType deftp_
              
instance CheckAccepter DefVar where
    accept (DefVar st typ name exp) checker = do typ_  <- visit checker typ
                                                 exp_  <- tryMaybe (visit checker) exp
                                                 return $ DefVar st typ_ name exp_
instance CheckAccepter DefFun where
    accept (DefFun st typ name params body) checker = do typ_  <- visit checker typ
                                                         ps_   <- visit checker params
                                                         body_ <- visit checker body
                                                         return $ DefFun st typ_ name ps_ body_

instance CheckAccepter DefStruct where                                                                
    accept (DefStruct name params) checker = do params_ <- tryAll (visit checker) params
                                                return $ DefStruct name params_

instance CheckAccepter DefUnion where                                                       
    accept (DefUnion name params) checker = do params_ <- tryAll (visit checker) params
                                               return $ DefUnion name params_

instance CheckAccepter DefType where                                                      
    accept (DefType typ name) checker = do typ_ <- visit checker typ
                                           return $ DefType typ_ name

instance CheckAccepter FuncParams where
    accept (VoidParams)         checker = do return VoidParams

    accept (FixedParams params) checker = do params_ <- tryAll (visit checker) params
                                             return $ FixedParams params_

    accept (VariableParams params) checker = do params_ <- tryAll (visit checker) params
                                                return $ VariableParams params_

instance CheckAccepter Param where
    accept (Param typ name) checker = do typ_ <- visit checker typ
                                         return $ Param typ_ name

instance CheckAccepter Block where
    accept (Block defvars statements) checker = do vars <- tryAll (visit checker) defvars
                                                   stms <- tryAll (visit checker) statements
                                                   return $ Block vars stms
                                                          
instance CheckAccepter Statement where
    accept (NulllineStatement) chk     = do return NulllineStatement
                                                   
    accept (LabeledStatement name) chk = do return $ LabeledStatement name
                                                   
    accept (ExpStatement exp) chk      = do exp_ <- visit chk exp
                                            return $ ExpStatement exp_
                                                   
    accept (BlockStatement block) chk  = do block_ <- visit chk block
                                            return $ BlockStatement block_
                                                   
    accept (IfStatement exp stA stB) chk = do exp_ <- visit chk exp
                                              stA_ <- visit chk stA
                                              stB_ <- visit chk stB
                                              return $ IfStatement exp_ stA_ stB_
                                                     
    accept (WhileStatement exp stmt) chk = do exp_  <- visit chk exp
                                              stmt_ <- visit chk stmt
                                              return $ WhileStatement exp_ stmt_

    accept (ForStatement exp1 exp2 exp3 stmt) chk = do exp1_ <- visit chk exp1
                                                       exp2_ <- visit chk exp2
                                                       exp3_ <- visit chk exp3
                                                       stmt_ <- visit chk stmt
                                                       return $ ForStatement exp1_ exp2_ exp3_ stmt_
            
    accept (BreakStatement)    chk = do return BreakStatement
    accept (ContinueStatement) chk = do return ContinueStatement
    accept (GotoStatement name) chk = do return $ GotoStatement name
    accept (ReturnStatement exp) chk = do exp_ <- visit chk exp
                                          return $ ReturnStatement exp_

instance CheckAccepter Expression where
    accept (AssignExp tm op val) chk = do tm_  <- visit chk tm
                                          val_ <- visit chk val
                                          return $ AssignExp tm_ op val_
    
    accept (TermExp tm) chk = do tm_ <- visit chk tm
                                 return $ TermExp tm_
    
    accept (NullExp) chk = do return NullExp
                              
    accept (TreeConditionExp exp1 exp2 exp3) chk = do e1 <- visit chk exp1
                                                      e2 <- visit chk exp2
                                                      e3 <- visit chk exp3
                                                      return $ TreeConditionExp e1 e2 e3

    accept (BoolCondExp exp1 exp2 op) chk = do e1 <- visit chk exp1
                                               e2 <- visit chk exp2
                                               return $ BoolCondExp e1 e2 op

    accept (BitcalcExp exp1 exp2 op) chk = do e1 <- visit chk exp1
                                              e2 <- visit chk exp2
                                              return $ BitcalcExp e1 e2 op

    accept (MathcalcExp exp1 exp2 op) chk = do e1 <- visit chk exp1
                                               e2 <- visit chk exp2
                                               return $ MathcalcExp e1 e2 op

instance CheckAccepter Term where
    accept (CastTerm typeref term) chk = do tp <- visit chk typeref
                                            tm <- visit chk term
                                            return $ CastTerm tp tm

    accept (PrefixCalcTerm pref term) chk = do tm <- visit chk term
                                               return $ PrefixCalcTerm pref tm

    accept (TypesizeTerm typeref) chk = do tp <- visit chk typeref
                                           return $ TypesizeTerm tp

    accept (SizeTerm term) chk = do tm <- visit chk term
                                    return $ SizeTerm tm

    accept (PostfixCalcTerm postfix term) chk = do tm <- visit chk term
                                                   return $ PostfixCalcTerm postfix tm

    accept (ArrayrefTerm exp term) chk = do ex <- visit chk exp
                                            tm <- visit chk term
                                            return $ ArrayrefTerm ex tm

    accept (StructrefTerm name term) chk = do tm <- visit chk term
                                              return $ StructrefTerm name tm

    accept (PointerrefTerm name term) chk = do tm <- visit chk term
                                               return $ PointerrefTerm name tm

    accept (FunccallTerm exps term) chk = do es <- tryAll (visit chk) exps
                                             tm <- visit chk term
                                             return $ FunccallTerm es tm

    accept (VariableTerm name entity) chk = return $ VariableTerm name entity

    accept (ExpLiteral exp) chk = do ep <- visit chk exp
                                     return $ ExpLiteral ep

    accept x chk = do return x

instance CheckAccepter Type where
    accept (Type base options) chk = do o <- tryAll (visit chk) options
                                        return $ Type base o
                                                 
instance CheckAccepter TyperefOption where
    accept (FuncPointerOption types) chk = do ts <- tryAll (visit chk) types
                                              return $ FuncPointerOption ts

    accept x chk = do return x
