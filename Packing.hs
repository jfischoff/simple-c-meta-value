module Packing where
import MetaValue where

version = 1 :: Word32

mk_header fixups = DB.runPut $ do
    DB.putWord32le version
    DB.putWord32le (fromIntegral $ length fixups :: Word32) 
    mapM_ DB.putWord32le fixups

packWord32 = DB.runPut . DB.putWord32le
unpackWord32 = DB.runGet DB.getWord32le

type Objects = [(Word32, Offset)]

type PackState = StateT Objects (StateT [Fixup] (StateT BS.ByteString Identity))

pack :: CEnv -> BS.ByteString
pack env = result where
    (((_, objects), fixups), bytes) = runIdentity $ runStateT (runStateT (runStateT (pack' env) []) []) BS.empty
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

replace_fixups bytes fixups objects = foldl (\b f -> replace_word32 b objects f) bytes fixups



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

pack_value :: Int -> CValue -> PackState () 
pack_value i v = do add_id i; pack_value' v

pack_value' :: CValue -> PackState ()
pack_value' (Struct members) = mapM_ pack_value' members
pack_value' (Union x i typ)  = pack_and_pad x $ fromIntegral $ size_of_t typ 
pack_value' (Primitive x)    = append_bytestring $ DB.encode x
pack_value' (Array xs)       = mapM_ pack_value' xs
pack_value' (Pointer (Id i)) = do 
        index <- fromIntegral <$> get_index
        add_fixup index
        append_bytestring $ packWord32 $ i
pack_value' (Member x count) = pack_and_pad x count 
 
pack_and_pad :: CValue -> Word32 -> PackState ()
pack_and_pad x count = do pack_value' x; pad count

pad_to amount x =  (((2 * amount) - (x `mod` amount)) `mod` amount) + x 

encode :: ToMetaValue a => a -> CEnv
encode x = result where 
    (value, state) = runState (to_c x) []
    result = (fromIntegral $ length state, value):state 

test_list_g = encode test_list
packed = pack test_list_g 

as_bytes = BS.unpack packed

as_bytes_a = BS.unpack $ pack $ encode $ test_a
as_bytes_b = BS.unpack $ pack $ encode $ test_b

write_file = BS.writeFile "test_objects.bin" (BS.append (packWord32 $ fromIntegral $ BS.length packed) packed)
