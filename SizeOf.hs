size_of_t (TStruct _ members)    = sum . map size_of_t $ members
size_of_t (TUnion x types)       = maximum $ map size_of_t types
size_of_t (TPrimitive x)         = size_of_tgl x
size_of_t (TArray count x)       = count * size_of_t x
size_of_t (TPointer _)           = 4
size_of_t (TMember _ x)          = pad_to 4 $ size_of_t x

size_of_tgl TGLbitfield = 4
size_of_tgl TGLboolean  = 1
size_of_tgl TGLbyte     = 1
size_of_tgl TGLchar     = 1
size_of_tgl TGLclampf   = 4
size_of_tgl TGLenum     = 4
size_of_tgl TGLfloat    = 4
size_of_tgl TGLint      = 4
size_of_tgl TGLshort    = 2
size_of_tgl TGLsizei    = 4
size_of_tgl TGLubyte    = 1
size_of_tgl TGLuint     = 4
size_of_tgl TGLushort   = 2

size_of (Struct members) = sum . map size_of $ members
size_of (Union x i typ)    = size_of_t typ
size_of (Primitive x)    = size_of_pgl x
size_of (Array xs)       = (fromIntegral . length $ xs) * size_of (head xs)
size_of (Pointer x)      = 4
size_of (Member x pad)   = pad_to 4 $ size_of x

size_of_pgl (PGLbitfield x) = 4
size_of_pgl (PGLboolean x)  = 1
size_of_pgl (PGLbyte x)     = 1
size_of_pgl (PGLchar x)     = 1
size_of_pgl (PGLclampf x)   = 4
size_of_pgl (PGLenum x)     = 4
size_of_pgl (PGLfloat x)    = 4
size_of_pgl (PGLint x)      = 4
size_of_pgl (PGLshort x)    = 2
size_of_pgl (PGLsizei x)    = 4
size_of_pgl (PGLubyte x)    = 1
size_of_pgl (PGLuint x)     = 4
size_of_pgl (PGLushort x)   = 2

