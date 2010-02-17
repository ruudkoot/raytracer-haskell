module Output where


import Colour


-- | Size should always be positive (i.e. natural),
-- so a newtype is needed. 
--
newtype Size = Size Integer deriving Eq

instance Show Size where 
  show (Size i) = show i

toSize :: Integral a => a -> Size 
toSize i = Size (max 0 (toInteger i))

fromSize :: Size -> Integer
fromSize (Size s) = s

type Width = Size
type Height = Size


type ImageWriter = Width -> Height -> Colours Int -> Maybe String

{-

* Problems with classes

So the following works:
 
 
class Show a => ImageWriter a where 
  toImage :: Width -> Height -> Colours Int -> Maybe a


But alas, you can't say:

class Show a => ImageWriter a where 
  toImage :: Width -> Height -> Colours Int -> Maybe String

Or: 

class Show a => ImageWriter a where 
  toImage :: Width -> Height -> Colours Int -> FilePath -> IO ()


More problems arise here:

class ImageWriter a => ImageSaver a where 
  saveImage :: FilePath -> a -> IO a
  saveImage fp s = (writeFile fp . show $ s) >> return s


We have to do something with ImageWriter a, 
to be able to derive the types and to let the 
compiler figure out which functions to use, 
but we don't need something of type ImageWriter a.
Illustrated here:


write :: (ImageWriter a, ImageSaver a) => Width -> Height -> Colours Int -> FilePath -> IO (Maybe a)
write w h cs fp = do return (toImage w h cs) >>= maybe (return Nothing) (fmap Just . saveImage fp)

Now we have to return the ImageWriter, which is kind of non-sensical. 

Then you can call write like this

write w h cs fp :: (IO (Maybe PPM))

So you still need to specify the type yourself, which kind of beats the point.

-}


