module Output.PPM (toPPM) where 

import Data.Char (chr)
import Data.Colour


-- | Size is a positive integer (i.e. a natural number)
--
newtype Size = Size Integer deriving Eq

instance Show Size where 
  show (Size i) = show i

fromSize :: Size -> Integer
fromSize (Size s) = s

toSize :: Int -> Size 
toSize = Size . fromIntegral

type Width = Size
type Height = Size


-- | Converts the colours to a String in PPM format.
-- Returns Nothing if the length of the list does 
-- not equal the given width times the given height. 
--
toPPM :: Width -> Height -> Colours Int -> Maybe String
toPPM w h cs = if fromSize w * fromSize h /= toInteger (length cs)
                 then Nothing
                 else Just $ concat ["P6\n#Team the ray team.\n", show w, " ", show h, "\n255\n",
                             concatMap (map chr . clampedList 0 255) cs]
