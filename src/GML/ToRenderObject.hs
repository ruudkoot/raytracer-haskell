module GML.ToRenderObject where

import           Base.Shape
import           Data.Angle
import           Data.Colour
import           Data.Transformation
import           Data.Texture
import           Data.Vector
import           Data.Glome.Vec hiding (translate, scale)

import Base.BoundingSphere

import qualified GML.AST        as GML
import qualified GML.Evaluate   as Evil
import qualified Renderer.Scene as Renderer

toRenderObject :: Textures -> GML.Object -> Renderer.Object
toRenderObject txs obj = GML.foldObject algebra obj $ identityTransformation
    where algebra = ( GML.SimpleTransformer $
              \shape closure trans   -> Renderer.Simple shape trans (Evil.shader txs closure) (boundingSphere shape)
            , \o d1 d2 d3 trans      -> updateBSphere (translateSphere d1 d2 d3)         $ o (trans !*! translate d1 d2 d3) 
            , \o d1 d2 d3 trans      -> updateBSphere (scaleSphere (max d1 (max d2 d3))) $ o (trans !*! scale d1 d2 d3)
            , \o d        trans      -> updateBSphere (scaleSphere d)                    $ o (trans !*! scale d d d)
            , \o d        trans      -> o (trans !*! rotateX (toRadians d))
            , \o d        trans      -> o (trans !*! rotateY (toRadians d))
            , \o d        trans      -> o (trans !*! rotateZ (toRadians d))
            , \o1 o2      trans      -> let o1' = o1 trans 
                                            o2' = o2 trans 
                                        in (Renderer.Union o1' o2' (unionSpheres (bsphere o1') (bsphere o2')))
            , \o1 o2      trans      -> let o1' = o1 trans 
                                            o2' = o2 trans 
                                        in (Renderer.Intersect o1' o2' (intersectSpheres (bsphere o1') (bsphere o2')))
            , \o1 o2      trans      -> let o1' = o1 trans 
                                            o2' = o2 trans 
                                        in (Renderer.Difference o1' o2' (differenceSpheres (bsphere o1') (bsphere o2')))
                                              
            )
                    
{-bbox :: Renderer.Object -> Bbox 
bbox (Renderer.Simple s   _ _ b) = b
bbox (Renderer.Union      _ _ b) = b 
bbox (Renderer.Intersect  _ _ b) = b
bbox (Renderer.Difference _ _ b) = b
-}

bsphere :: Renderer.Object -> BSphere
bsphere (Renderer.Simple s   _ _ b) = b
bsphere (Renderer.Union      _ _ b) = b 
bsphere (Renderer.Intersect  _ _ b) = b
bsphere (Renderer.Difference _ _ b) = b

updateBSphere::(BSphere->BSphere) -> Renderer.Object -> Renderer.Object
updateBSphere f (Renderer.Simple s   sh t  b) = Renderer.Simple s sh t (f b)
updateBSphere f (Renderer.Union      o1 o2 b) = Renderer.Union o1 o2 (f b)
updateBSphere f (Renderer.Intersect  o1 o2 b) = Renderer.Intersect o1 o2 (f b)
updateBSphere f (Renderer.Difference o1 o2 b) = Renderer.Difference o1 o2 (f b)

transformBbox :: Bbox -> Transformation -> Bbox 
transformBbox (Bbox p1 p2) trans = Bbox (toVec3D (min x1 x2) (min y1 y2) (min z1 z2))
                                        (toVec3D (max x1 x2) (max y1 y2) (max z1 z2))
  where (Vec x1 y1 z1) = invxfm_vec trans p1 
        (Vec x2 y2 z2) = invxfm_vec trans p2

toWorld :: Textures -> GML.Scene -> Renderer.Scene
toWorld txs (GML.Scene amb l obj dp fov w h fil) = 
       Renderer.Scene (Renderer.Options (toColour amb) dp (toRadians fov) w h fil) (toRenderObject txs obj) l

