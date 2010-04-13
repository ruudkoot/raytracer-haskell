module GML.ToRenderObject where

import           Base.Shape
import           Data.Angle
import           Data.Colour
import           Data.Transformation
import           Data.Texture
import           Data.Glome.Vec (Bbox(..), bbjoin, bboverlap, xfm_vec) 

import qualified GML.AST        as GML
import qualified GML.Evaluate   as Evil
import qualified Renderer.Scene as Renderer

toRenderObject :: Textures -> GML.Object -> Renderer.Object
toRenderObject txs = flip (GML.foldObject algebra) identityTransformation
    where algebra = ( GML.SimpleTransformer $
              \shape closure trans -> Renderer.Simple shape trans (Evil.shader txs closure) (transformBbox (boundingBox shape) trans)
            , \o d1 d2 d3    trans -> o (trans !*! translate d1 d2 d3)
            , \o d1 d2 d3    trans -> o (trans !*! scale d1 d2 d3)
            , \o d           trans -> o (trans !*! scale d d d)
            , \o d           trans -> o (trans !*! rotateX (toRadians d))
            , \o d           trans -> o (trans !*! rotateY (toRadians d))
            , \o d           trans -> o (trans !*! rotateZ (toRadians d))
            , \o1 o2         trans -> Renderer.Union      (o1 trans) (o2 trans) (bbjoin (bbox (o1 trans) trans) (bbox (o2 trans) trans))
            , \o1 o2         trans -> Renderer.Intersect  (o1 trans) (o2 trans) (bboverlap (bbox (o1 trans) trans) (bbox (o2 trans) trans))
            , \o1 o2         trans -> Renderer.Difference (o1 trans) (o2 trans) (bbjoin (bbox (o1 trans) trans) (bbox (o2 trans) trans))
            )
                    
bbox :: Renderer.Object -> Transformation -> Bbox 
bbox (Renderer.Simple s   _ _ b) = transformBbox b
bbox (Renderer.Union      _ _ b) = transformBbox b 
bbox (Renderer.Intersect  _ _ b) = transformBbox b
bbox (Renderer.Difference _ _ b) = transformBbox b

transformBbox :: Bbox -> Transformation -> Bbox 
transformBbox (Bbox p1 p2) trans = Bbox (transformPoint trans p1) (transformPoint trans p2)

toWorld :: Textures -> GML.Scene -> Renderer.Scene
toWorld txs (GML.Scene amb l obj dp fov w h fil) = 
       Renderer.Scene (Renderer.Options (toColour amb) dp (toRadians fov) w h fil) (toRenderObject txs obj) l

