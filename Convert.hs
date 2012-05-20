{-# LANGUAGE DeriveGeneric, TypeOperators, FlexibleInstances, TypeSynonymInstances,
    DefaultSignatures, FlexibleContexts, DeriveDataTypeable, StandaloneDeriving, NoMonomorphismRestriction #-}
module Conversion where
import GHC.Generics
import Language.C.Simple.Value
import Data.Tuple.Select
import Data.Maybe
import GLPrimitives
import Control.Applicative
import Control.Monad
import Control.Monad.State
import qualified Data.ByteString.Lazy as BS
import TemplateReflect
import Control.Arrow
import Data.Word
import Data.Data
import Data.Typeable


            
add_array value = do
    count <- gets length
    modify ((count, value):)
    return $ fromIntegral count

-- | This an important function..
encode :: (ToCType a, ToValue a) => a -> CEnv
encode x = result where
    (value, state) = runState (to_c_value (to_c_type x) (to_v x)) []
    result = map (second wrap_with_type_struct) ((fromIntegral $ length state, value):state)

wrap_with_type_struct :: CValue -> CValue
wrap_with_type_struct (Union s@(Struct xs) 0 (TUnion _ (typ:[]))) = s
wrap_with_type_struct u@(Union s index (TUnion _ (x:xs))) = Struct [
                        Primitive . PGLenum $ GLenum index,
                        u
                    ]
wrap_with_type_struct x = x

fromTMember (TMember name x) = (name, x)

find_type :: String -> [CType] -> (Word32, CType)
find_type x t_members = (\(i, (_,x)) -> (i, x)) $ head $ 
    filter ((x==) . fst . snd) $ zip [0..] $ map fromTMember t_members

rootMember = flip member 0
union' u (i, cv) = Union cv i u

--I need the description so I can properly create unions
to_c_value :: CType -> Value -> CState CMetaValue
to_c_value   (TPrimitive _       ) (VPrimitive x     ) = return      $  Primitive x
to_c_value   (TStruct _ t_members) (VStruct v_members) = Struct     <$> zipWithM to_c_value t_members v_members
to_c_value   (TArray count typ   ) (VArray xs        ) = Array      <$> zipWithM to_c_value (repeat typ) xs
to_c_value   (TMember _ x        ) (VMember y        ) = rootMember <$> to_c_value x y
to_c_value u@(TUnion _ t_members ) (VUnion name x    ) = union' u   <$> join $ (second (flip to_c_value x)) $ find_type name t_members
to_c_value   (TPointer typ       ) (VArray xs        ) = do
    --Type the elements
    ys <- zipWithM to_c_value (cycle [typ]) xs 
    --add the array as a resource
    id <- add_array (Array ys)
    --use the resource identitier
    return $ Struct [Pointer $ Id id, Primitive $ PGLuint $ fromIntegral $ length ys]
















