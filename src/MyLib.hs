module MyLib where

import Foreign.C (CSize (..))
import System.IO.Unsafe

foreign import ccall unsafe "rs_hello_world"
  rs_hello_world :: IO ()

foreign import ccall unsafe "rs_add"
  rs_add :: CSize -> CSize -> IO CSize

add :: Word -> Word -> Word
add lhs rhs = unsafePerformIO $ do 
    res <- rs_add (fromIntegral lhs) (fromIntegral rhs)
    pure (fromIntegral res)
