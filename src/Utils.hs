module Utils where

import Data.Word (Word32)

type Position = (Word32, Word32)

lengthW :: (Num w) => [a] -> w
lengthW = fromIntegral . length

type Positioned a = (a, Position)
