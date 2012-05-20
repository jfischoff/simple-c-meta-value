module Main where
    
instance ToValue TestObject    
    
--this needs to be done with 
instance ToCType TestObject where
    to_c_type x = mk_record_ctype "TestObject" [
        mk_primitive_member "x" TGLint,
        mk_primitive_member "y" TGLchar,
        mk_primitive_member "z" TGLfloat]

instance ToValue TestList

instance ToCType TestList where
    to_c_type x = mk_record_ctype "TestList" [
        mk_primitive_member "test_list_x" TGLchar,
        TMember "list" $ TPointer $ to_c_type (undefined :: TestObject),
        mk_primitive_member "list_count" TGLuint]
        
data ManyC = Hey Int
           | You Char
           deriving(Typeable, Data)
           
data TestObject = TestObject 
   {
       x :: GLint,
       y :: GLchar,
       z :: GLfloat
   }
   deriving(Show, Eq, Generic, Data, Typeable)



instance ToCValue TestObject where    
   to_c (TestObject x y z) = do
       x_c <- to_c x 
       y_c <- to_c y 
       z_c <- to_c z
       return $ Struct [x_c, y_c, z_c]

data TestList = TestList 
   {
       test_list_x :: GLchar,
       list :: [TestObject]
   }
   deriving(Show, Eq, Generic, Data, Typeable)

instance ToCValue TestList where
   to_c (TestList x y) = do
       x_c <- to_c x
       ys <- mapM to_c y
       id <- add_array (Array ys)
       z_c <- to_c (fromIntegral $ length y :: GLint) 
       return $ Struct [x_c, Pointer $ Id id, z_c]
       
       
test_a = TestObject 438129054 (GLchar 'a') 1.5
test_b = TestObject 102 (GLchar 'b') 5.5

test_list = TestList (GLchar 't') [test_a, test_b]