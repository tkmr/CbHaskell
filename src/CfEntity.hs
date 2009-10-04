module CfEntity where
import {-# SOURCE #-} qualified CfDataType as D

---Entity
data Entity = DefVarEntity EntityBase D.DefVar
            | ConstEntity  EntityBase D.Type

class Entiter a where
    toEntity :: a -> Entity

instance Entiter D.DefVar where
    toEntity defvar = DefVarEntity EmptyEntityBase defvar

data EntityBase = EntityBase String
                | EmptyEntityBase


