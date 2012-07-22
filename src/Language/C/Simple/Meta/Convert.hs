{-# LANGUAGE DeriveGeneric, TypeOperators, FlexibleInstances, TypeSynonymInstances,
    DefaultSignatures, FlexibleContexts, DeriveDataTypeable, StandaloneDeriving, NoMonomorphismRestriction #-}
module Language.C.Simple.Meta.Convert where
import Language.C.Simple.Meta.Types
import Language.C.Simple.Meta.Class
import GHC.Generics
import Language.C.Simple.CValue
import Language.C.Simple.CType
import Data.Tuple.Select
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Monad.State
import qualified Data.ByteString.Lazy as BS
import Control.Arrow
import Data.Word
import Data.Data
import Data.Typeable
import Generics.Pointless.MonadCombinators
   
   

   
          
add_array value = do
    count <- gets length
    modify ((count, value):)
    return $ fromIntegral count

-- | This an important function..
encode :: (ToCType a, ToCValue a) => a -> CEnv
encode x = result where
    (value, state) = runState (defToMetaValue (toCType x) (toCValue x)) []
    result = map (second wrap_with_type_struct) 
                    ((fromIntegral $ length state, value):state)

member = error "undefined"

wrap_with_type_struct :: MetaValue -> MetaValue
--wrap_with_type_struct (VUnion s@(VStruct xs) 0 (TUnion _ (typ:[]))) = s
--wrap_with_type_struct u@(VUnion s index (TUnion _ (x:xs))) = VStruct [
--                        Primitive . PGLenum $ GLenum index, u]
--wrap_with_type_struct x = x
wrap_with_type_struct = error "wrap_with_type_struct"

fromTMember (TMember name x) = (name, x)

find_type :: UnionPath -> [CType] -> (Word32, CType)
find_type x t_members = (fromIntegral index, typ) where 
    index = toTypeIndex x (length t_members)
    typ   = snd $ fromTMember (t_members !! index)

toTypeIndex :: UnionPath -> Int -> Int
toTypeIndex path count = error "toTypeIndex"

rootMember = flip member 0


toPrimitive = error "toPrimitive"

type Result = Either String


--I need the description so I can properly create unions

defToMetaValue :: CType -> CValue -> CState MetaValue
defToMetaValue   (TPrimitive _       ) (VPrimitive x     ) = return $ Primitive x
defToMetaValue   (TStruct _ t_members) (VStruct v_members) = 
    Struct     <$> zipWithM defToMetaValue t_members v_members
defToMetaValue   (TArray count typ   ) (VArray xs        ) = 
    Array      <$> zipWithM defToMetaValue (repeat typ) xs
defToMetaValue   (TMember _ x        ) (VMember y        ) = 
    rootMember <$> defToMetaValue x y
defToMetaValue u@(TUnion _ t_members ) (VUnion name x    ) = do 
        let (index, typ) = find_type name t_members
        value <- defToMetaValue typ x
        return $ Union t_members index value
defToMetaValue   (TPointer typ       ) (VArray xs        ) = do
    --Type the elements
    ys <- zipWithM defToMetaValue (cycle [typ]) xs 
    --add the array as a resource
    id <- add_array (Array ys)
    --use the resource identitier
    return $ Struct [Pointer $ Id id, Primitive $ PInt $ fromIntegral $ length ys]
















