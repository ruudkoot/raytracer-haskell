{-# LANGUAGE ParallelListComp,RankNTypes, GADTs,TypeSynonymInstances #-}
module Data.Texture where

import Data.List.Split
import Data.List 

import Data.Bitmap
import Codec.Image.STB

import Data.Colour
import qualified Data.Map as M

import qualified Data.ByteString as BS
import Data.Array

import Control.Monad
import Control.Monad.Error

type TextureRef = String
type Textures = M.Map TextureRef Texture
type Texture = TextureArray

--Plain textures
class Texture2D a where
    getLinear::a->(Double,Double)->ColourD
    getLinear t (x,y) = --getat t (floor x, floor y)
                        let lx = floor x --Square boundarys
                            hx = ceiling x
                            ly = floor y
                            hy = ceiling y
                            tl = getat t (lx,ly) --Corner colors
                            tr = getat t (hx,ly)
                            bl = getat t (lx,hy)
                            br = getat t (hx,hy)
                            dx = x - fromIntegral lx --Distances to square boundary topleft
                            dy = y - fromIntegral ly
                            top = fmap ((1.0-dx)*) tl `addColour` fmap (dx*) tr
                            bottom = fmap ((1.0-dx)*) bl `addColour` fmap (dx*) bl
                         in fmap ((1.0-dy)*) top `addColour` fmap (dy*) bottom

    getLinear'::a->(Double,Double)->ColourD
    getLinear' t (x,y) = let (w,h) = getDimension t
                         in getLinear t (x*fromIntegral w,y*fromIntegral h)

    getat::a->(Int,Int)->ColourD
    getDimension::a->(Int,Int)
--    createMipmap::a->a

--Array-based texture
type TextureArray = Array (Int,Int) ColourD

instance Texture2D TextureArray where
    getDimension = snd.bounds
    getat tx (x,y) = let (w,h) = getDimension tx
                     in tx ! (1+(x `mod` w), 1+(y `mod` h))

loadTexture:: FilePath -> IO TextureArray
loadTexture file = 
   do e <- loadImage file
      let img = either (\e -> error $ "Error loading texture: "++file++" - "++e) id e
      let chans = bitmapNChannels img
      guard (chans == 3)
      let tx = bitmapToTextureArray img
      putStrLn $ "Texture loaded: "++file
      putStrLn $ "Dimensions: "++show (getDimension tx)
--      putStrLn $ "     
      return tx

bitmapToTextureArray::Bitmap Word8 -> TextureArray
bitmapToTextureArray img = 
    let (w,h) = bitmapSize img
        bs = bitmapToByteString img
        convertColor::BS.ByteString -> ColourD
        convertColor = fromRGB.listToColour.map fromIntegral. BS.unpack
        convertBS::BS.ByteString -> [ColourD]
        convertBS bs | BS.null bs = []
                     | otherwise  =  let (clr, rest) = BS.splitAt 3 bs
                                     in convertColor clr:convertBS rest
        lData = convertBS (bitmapToByteString img)
        transData = concat.transpose.splitEvery w $ lData
    in listArray ((1,1),(w,h)) transData


--list-based texture
data TextureList= TextureList
    {width::Int
    ,height::Int
    ,pixels::[[ColourD]]
    }

instance Texture2D TextureList where
    getDimension lt = (width lt, height lt)
    getat lt (x,y) = let (w,h) = getDimension lt
                     in (pixels lt !! (y `mod` h)) !! (x `mod` w)
{-    createMipmap lt = let (nw,nh)=(width lt `div` 2, height lt `div` 2)
                          npixels = [[getLinear lt (fromIntegral x, fromIntegral y) | x <- [1..nw]] | y <- [1..nh]]
                      in TextureList nw nh npixels-}




   
--Mipmapped textures
{-class MTexture2D a where
    createMipmaps::a->a
    sampleMipmap::a->(Double,Double,Double)->ColourD
    sampleMipmap'::a->(Double,Double,Double)->ColourD

data MMTexture = MMTexture
    {source::forall a.(Texture2D a) => a
    ,mipmaps::forall a.(Texture2D a) => [a]
    ,nmipmaps::Int
    }

instance MTexture2D MMTexture where
    createMipmaps (MMTexture s _ n) = 
        let cmipmaps 0 _ = []
            cmipmaps n tx = tx: cmipmaps (n-1) (createMipmap tx)
        in MMTexture s (cmipmaps n s) n        
    sampleMipmap tx (x,y,z) = let (w,h) = getDimension (source tx)
                              in sampleMipmap' tx (x/fromIntegral w, y/fromIntegral h,z)
--    sampleMipmap' tx (x,y,z) = -}
