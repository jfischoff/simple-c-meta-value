{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Language.C.Simple.Meta.Types where
import Language.C.Simple.CType
import Language.C.Simple.CValue
import Data.Data
import GHC.Generics
import Data.Word

-- | A mix of type and value information.
data MetaValue = Struct [MetaValue]
               | Union [CType] Word32 MetaValue 
               | Primitive PrimitiveValue
               | Array [MetaValue]
               | Pointer MetaValue
               | Member MetaValue Word32
               | Id Word32
               | Enum [(String, Word32)]
               | EmptyValue
             deriving(Show, Eq, Read, Ord, Data, Typeable, Generic)



