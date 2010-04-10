module Input.GML.ToRenderObject where

import           Data.Angle
import           Data.Colour
import           Data.Matrix
import           Data.Texture

import qualified Input.GML.AST      as GML
import qualified Input.GML.Evaluate as Evil
import qualified Renderer.Scene     as Renderer

toRenderObject :: Textures -> GML.Object -> Renderer.Object
toRenderObject txs = flip (GML.foldObject algebra) identityTransformation
    where algebra = ( GML.SimpleTransformer $
                      \shape closure transformation -> Renderer.Simple shape transformation (Evil.shader txs closure)
                    , \o d1 d2 d3    transformation -> o (multiplyTransformations (translate d1 d2 d3) transformation)
                    , \o d1 d2 d3    transformation -> o (multiplyTransformations (scale d1 d2 d3) transformation)
                    , \o d           transformation -> o (multiplyTransformations (scale d d d) transformation)
                    , \o d           transformation -> o (multiplyTransformations (rotateX (toRadians d)) transformation)
                    , \o d           transformation -> o (multiplyTransformations (rotateY (toRadians d)) transformation)
                    , \o d           transformation -> o (multiplyTransformations (rotateZ (toRadians d)) transformation)
                    , \o1 o2         transformation -> Renderer.Union      (o1 transformation) (o2 transformation)
                    , \o1 o2         transformation -> Renderer.Intersect  (o1 transformation) (o2 transformation)
                    , \o1 o2         transformation -> Renderer.Difference (o1 transformation) (o2 transformation)
                    )
                    
toWorld :: Textures -> GML.Scene -> Renderer.World
toWorld txs (GML.Scene amb l obj dp fov w h fil) = 
       Renderer.World (Renderer.RenderOptions (toColour amb) dp (toRadians fov) w h fil) (toRenderObject txs obj) l

