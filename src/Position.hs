module Position where

import Data.Word (Word32)

type Position = (Word32, Word32)

type Positioned a = (a, Position)

lengthW :: (Num w) => [a] -> w
lengthW = fromIntegral . length
