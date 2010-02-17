module PPM where 

import Colour
import Output 


-- | Converts the colours to a String in PPM format.
-- Returns Nothing if the length of the list does 
-- not equal the given width times the given height. 
--
toPPM :: ImageWriter
toPPM w h cs = if fromSize w * fromSize h /= toInteger (length cs)
                 then Nothing
                 else Just $ concat ["P3 ", show w, " ", show w, " 255\n",
                             unlines (map (unwords . map show . clampedList 0 255) cs)]
