module HaskRs where

import Foreign.C (CSize (..))
import System.IO.Unsafe

-- | Straightforward simple wrapper
-- which prints a string to stdout
foreign import ccall unsafe "rs_hello_world"
  helloWorld :: IO ()

-- | Slightly more complex example
-- where we do some input/output conversion
-- and call a morally-pure function
-- (thus handwaving the IO away).
add :: Word -> Word -> Word
add lhs rhs = unsafeDupablePerformIO $ do 
    res <- rs_add (fromIntegral lhs) (fromIntegral rhs)
    pure (fromIntegral res)

foreign import ccall unsafe "rs_add"
  rs_add :: CSize -> CSize -> IO CSize

