
-- | A mix of type and value information.
data MetaValue = Struct [MetaValue]
             | Union CValue Word32 CType
             | Primitive GLPrimitive
             | Array [MetaValue]
             | Pointer MetaValue
             | Member MetaValue Word32
             | Id Word32
             | Enum [(String, Word32)]
             | EmptyValue
             deriving(Show, Data, Typeable)


type CEnv = [(Int, MetaValue)]

type CState = State CEnv
