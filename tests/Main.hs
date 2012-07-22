{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TemplateHaskell #-}
module Main where
import Language.C.Simple.Meta
import Language.C.Simple.CType.Build.TH
import Language.C.Simple.CType
import qualified Data.ByteString.Lazy as BS
import Data.Typeable
import Data.Data
import GHC.Generics
import Foreign.C.Types
import Data.Char


    
--this needs to be done with 
--instance ToCType TestObject where
--    toCType x = mk_record_ctype "TestObject" [
--        mk_primitive_member "x" TInt,
--        mk_primitive_member "y" TChar,
--        mk_primitive_member "z" TFloat]


        
data ManyC = Hey Int
           | You Char
           deriving(Typeable, Data)
           
data TestObject = TestObject 
   {
       x :: CInt,
       y :: CChar,
       z :: CFloat
   }
   deriving(Show, Eq, Generic, Data, Typeable)

instance ToCValue TestObject    



instance ToMetaValue TestObject where    
   toMetaValue (TestObject x y z) = do
       x_c <- toMetaValue x 
       y_c <- toMetaValue y 
       z_c <- toMetaValue z
       return $ Struct [x_c, y_c, z_c]

data TestList = TestList 
   {
       test_list_x :: CChar,
       list :: [TestObject]
   }
   deriving(Show, Eq, Generic, Data, Typeable)

instance ToCValue TestList

instance ToCType TestList where
   toCType x = mkRecordCType "TestList" [
       mkPrimitiveMember "test_list_x" TChar,
       TMember "list" $ TPointer $ toCType (undefined :: TestObject),
       mkPrimitiveMember "list_count" TUInt]

instance ToMetaValue TestList where
   toMetaValue (TestList x y) = do
       x_c <- toMetaValue x
       ys <- mapM toMetaValue y
       id <- add_array (Array ys)
       z_c <- toMetaValue (fromIntegral $ length y :: CInt) 
       return $ Struct [x_c, Pointer $ Id id, z_c]
       
       
test_a = TestObject 438129054 (read "a") 1.5
test_b = TestObject 102 (read "b") 5.5

test_list = TestList (read "t") [test_a, test_b]

as_bytes_a = BS.unpack $ pack $ encode $ test_a
as_bytes_b = BS.unpack $ pack $ encode $ test_b

write_file = BS.writeFile "test_objects.bin" (BS.append (packWord32 $ fromIntegral $ BS.length packed) packed)

packed = pack test_list_g 
test_list_g = encode test_list
as_bytes = BS.unpack packed


$(mk_simple_c_type_record ''TestObject)