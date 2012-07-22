module Language.C.Simple.Meta.Packing where
import Language.C.Simple.Meta.Types 
import Language.C.Simple.Meta.Utils
import Language.C.Simple.CValue
import Language.C.Simple.CType
import Language.C.Simple.Meta.Class
import Language.C.Simple.Meta.SizeOf
import Language.C.Simple.Meta.Convert
import Data.Int
import Control.Monad.Identity
import qualified Data.ByteString.Lazy as BS
import qualified Data.Binary as DB
import qualified Data.Binary.Put as DB
import qualified Data.Binary.Get as DB
import Control.Monad.State
import Data.Word
import Control.Applicative
import Debug.Trace.Helpers
import Control.Arrow
import Data.Maybe



version = 1 :: Word32
type Offset = Word32
type Fixup = Word32

mk_header fixups = DB.runPut $ do
    DB.putWord32le version
    DB.putWord32le (fromIntegral $ length fixups :: Word32) 
    mapM_ DB.putWord32le fixups

packWord32   = DB.runPut . DB.putWord32le
unpackWord32 = DB.runGet DB.getWord32le

type Objects = [(Word32, Offset)]

type PackState = StateT Objects (StateT [Fixup] (StateT BS.ByteString Identity))

pack :: CEnv -> BS.ByteString
pack env = result where
    (((_, objects), fixups), bytes) = 
        runIdentity $ runStateT (runStateT (runStateT (pack' env) []) []) BS.empty
    header = mk_header fixups
    new_fixups  = map ((fromIntegral $ BS.length header)+) fixups
    new_header  = mk_header $ traceIt new_fixups
    new_objects = offset_objects (fromIntegral $ BS.length new_header) objects
    new_bytes  = replace_fixups bytes fixups $ new_objects
    result = BS.append new_header new_bytes
    
offset_objects offset objects = map (second (offset+)) objects
    
replace_word32 :: BS.ByteString -> Objects -> Word32 -> BS.ByteString
replace_word32 bytes objects index = result where
    --split at the start
    (start, end) = BS.splitAt (fromIntegral index) bytes
    --split after
    (word_bytes, end') = BS.splitAt 4 end
    --turn the 4 bytes into a word
    word = unpackWord32 word_bytes :: Word32
    --add the offset 
    replacement_word = fromJust $ lookup word objects
    --convert back to bytes
    new_word_bytes = packWord32 replacement_word
    --reconnect everything
    result = BS.concat [start, new_word_bytes, end']

replace_fixups bytes fixups objects = 
    foldl (\b f -> replace_word32 b objects f) bytes fixups



add_fixup :: Fixup -> PackState ()
add_fixup x = lift $ modify (x:)

get_index :: PackState Int32
get_index = lift $ lift $ gets $ fromIntegral . BS.length

append_bytestring :: BS.ByteString -> PackState ()
append_bytestring x = lift $ lift $ modify $ (flip BS.append) x

pad :: Word32 -> PackState ()
pad count = append_bytestring $ BS.pack $ take (fromIntegral count) (cycle [0])

pack' :: CEnv -> PackState ()
pack' env = mapM_ (\(i, v) -> pack_value i v) env

add_id i = do 
    index <- get_index
    modify ((fromIntegral i, fromIntegral index):)

pack_value :: Int -> MetaValue -> PackState () 
pack_value i v = do add_id i; pack_value' v

encodePrimitive = error "encodePrimitive"

pack_value' :: MetaValue -> PackState ()
pack_value' (Struct members) = mapM_ pack_value' members
pack_value' (Union typ i x)  = pack_and_pad x $ fromIntegral $ sum $ map sizeOf typ 
pack_value' (Primitive x)    = append_bytestring $ encodePrimitive x
pack_value' (Array xs)       = mapM_ pack_value' xs
pack_value' (Pointer (Id i)) = do 
        index <- fromIntegral <$> get_index
        add_fixup index
        append_bytestring $ packWord32 $ i
pack_value' (Member x count) = pack_and_pad x count 
 
pack_and_pad :: MetaValue -> Word32 -> PackState ()
pack_and_pad x count = do pack_value' x; pad count

 

--encode :: ToMetaValue a => a -> CEnv
--encode x = result where 
--   (value, state) = runState (toMetaValue x) []
--    result = (fromIntegral $ length state, value):state 







