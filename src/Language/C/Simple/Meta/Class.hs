module Language.C.Simple.Meta.Class where
import Language.C.Simple.Meta.Types
import Language.C.Simple.CValue
import Control.Monad.State
import Foreign.C.Types

type CState = State CEnv    
type CEnv   = [(Int, MetaValue)]    
    
class ToMetaValue a where
    toMetaValue :: a -> CState MetaValue
    
------------------------------------------------------------------------------------
-- Primitive Instances
------------------------------------------------------------------------------------
instance ToMetaValue CFloat where
    toMetaValue = return . Primitive . PFloat 

instance ToMetaValue CDouble where
    toMetaValue = return . Primitive . PDouble 

instance ToMetaValue CChar where
    toMetaValue = return . Primitive . PChar

instance ToMetaValue CSChar where
    toMetaValue = return . Primitive . PSChar

instance ToMetaValue CUChar where
    toMetaValue = return . Primitive . PUChar

instance ToMetaValue CShort where
    toMetaValue = return . Primitive . PShort

instance ToMetaValue CUShort where
    toMetaValue = return . Primitive . PUShort

instance ToMetaValue CInt where
    toMetaValue = return . Primitive . PInt

instance ToMetaValue CUInt where
    toMetaValue = return . Primitive . PUInt

instance ToMetaValue CLong where
    toMetaValue = return . Primitive . PLong

instance ToMetaValue CULong where
    toMetaValue = return . Primitive . PULong

instance ToMetaValue CPtrdiff where
    toMetaValue = return . Primitive . PPtrdiff

instance ToMetaValue CSize where
    toMetaValue = return . Primitive . PSize

instance ToMetaValue CWchar where
    toMetaValue = return . Primitive . PWchar

instance ToMetaValue CSigAtomic where
    toMetaValue = return . Primitive . PSigAtomic

instance ToMetaValue CLLong where
    toMetaValue = return . Primitive . PLLong

instance ToMetaValue CULLong where
    toMetaValue = return . Primitive . PULLong

instance ToMetaValue CIntPtr where
    toMetaValue = return . Primitive . PIntPtr

instance ToMetaValue CUIntPtr where
    toMetaValue = return . Primitive . PUIntPtr

instance ToMetaValue CIntMax where
    toMetaValue = return . Primitive . PIntMax

instance ToMetaValue CUIntMax where
    toMetaValue = return . Primitive . PUIntMax

instance ToMetaValue CClock where
    toMetaValue = return . Primitive . PClock

instance ToMetaValue CTime where
    toMetaValue = return . Primitive . PTime

instance ToMetaValue CUSeconds where
    toMetaValue = return . Primitive . PUSeconds

instance ToMetaValue CSUSeconds where
    toMetaValue = return . Primitive . PSUSeconds    