module Output.PPM (toPPM) where 

import Data.Char (chr)
import Output.Output 

-- | Converts the colours to a String in PPM format.
-- Returns Nothing if the length of the list does 
-- not equal the given width times the given height. 
--
toPPM :: ImageWriter
toPPM w h cs = if fromSize w * fromSize h /= toInteger (length cs)
                 then Nothing
                 else Just $ concat ["P6\n#Team the ray team.\n", show w, " ", show h, "\n255\n",
                             concatMap (map chr . (clampedList 0 255)) cs]
