module Language.C.Simple.Meta.SizeOf where
import Language.C.Simple.CType
import Language.C.Simple.CValue
import Language.C.Simple.Meta.Utils

-- | a class for determining the size of a value
--   TODO: this should take in a word size and alignment
class SizeOf a where
    sizeOf :: a -> Int

instance SizeOf CType where
    sizeOf (TStruct _ members)    = sum . map sizeOf $ members
    sizeOf (TUnion x types)       = maximum $ map sizeOf types
    sizeOf (TPrimitive x)         = sizeOf x
    sizeOf (TArray count x)       = count * sizeOf x
    sizeOf (TPointer _)           = 4
    sizeOf (TMember _ x)          = padTo 4 $ sizeOf x

instance SizeOf PrimitiveType where
    sizeOf TChar      = 4
    sizeOf TSChar     = 1
    sizeOf TUChar     = 1
    sizeOf TShort     = 1
    sizeOf TUShort    = 4
    sizeOf TInt       = 4
    sizeOf TUInt      = 4
    sizeOf TLong      = 4
    sizeOf TULong     = 2
    sizeOf TPtrdiff   = 4
    sizeOf TSize      = 1
    sizeOf TWchar     = 4
    sizeOf TSigAtomic = 2
    sizeOf TLLong     = 4
    sizeOf TULLong    = 1
    sizeOf TIntPtr    = 1
    sizeOf TUIntPtr   = 1
    sizeOf TIntMax    = 4
    sizeOf TUIntMax   = 4
    sizeOf TClock     = 4
    sizeOf TTime      = 4
    sizeOf TUSeconds  = 2
    sizeOf TSUSeconds = 4
    sizeOf TFloat     = 1
    sizeOf TDouble    = 4
    sizeOf TVoid      = 2
    


