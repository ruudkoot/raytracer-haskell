{-# LANGUAGE ParallelListComp,RankNTypes, GADTs #-}
module Data.Texture where
import Data.Colour

class Texture2D a where
    getLinear::a->(Double,Double)->ColourD
    getLinear t (x,y) = let lx = floor x --Square boundarys
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
    createMipmap::a->a

class MTexture2D a where
    createMipmaps::a->a
    sampleMipmap::a->(Double,Double,Double)->ColourD
    sampleMipmap'::a->(Double,Double,Double)->ColourD

data ListTexture = ListTexture
    {width::Int
    ,height::Int
    ,pixels::[[ColourD]]
    }

instance Texture2D ListTexture where
    getDimension lt = (width lt, height lt)
    getat lt (x,y) = let (w,h) = getDimension lt
                     in (pixels lt !! (y `mod` h)) !! (x `mod` w)
    createMipmap lt = let (nw,nh)=(width lt `div` 2, height lt `div` 2)
                          npixels = [[getLinear lt (fromIntegral x, fromIntegral y) | x <- [1..nw]] | y <- [1..nh]]
                      in ListTexture nw nh npixels

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
--    sampleMipmap' tx (x,y,z) = 
