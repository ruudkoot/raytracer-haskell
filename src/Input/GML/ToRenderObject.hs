module Input.GML.ToRenderObject where

import           Data.Matrix
import           Data.Vector
import           Data.Colour
import           Data.Radians

import qualified Input.GML.AST      as GML
import qualified Input.GML.Evaluate as Evil
import qualified Renderer.Scene     as Renderer

toRenderObject :: GML.Object -> Renderer.Object
toRenderObject = flip (GML.foldObject algebra) identityTransformation
    where algebra = ( \shape closure transformation -> Renderer.Simple shape transformation (Evil.shader closure)
                    , \o d1 d2 d3    transformation -> o (multiplyTransformations (translate d1 d2 d3) transformation)
                    , \o d1 d2 d3    transformation -> o (multiplyTransformations (scale d1 d2 d3) transformation)
                    , \o d           transformation -> o (multiplyTransformations (scale d d d) transformation)
                    , \o d           transformation -> o (multiplyTransformations (rotateX (radians d)) transformation)
                    , \o d           transformation -> o (multiplyTransformations (rotateY (radians d)) transformation)
                    , \o d           transformation -> o (multiplyTransformations (rotateZ (radians d)) transformation)
                    , \o1 o2         transformation -> Renderer.Union      (o1 transformation) (o2 transformation)
                    , \o1 o2         transformation -> Renderer.Intersect  (o1 transformation) (o2 transformation)
                    , \o1 o2         transformation -> Renderer.Difference (o1 transformation) (o2 transformation)
                    )

toWorld::GML.Scene->Renderer.World
toWorld (GML.Scene amb l obj dp fov w h fil) = 
       Renderer.World (Renderer.RenderOptions (toColour amb) dp fov w h fil) (toRenderObject obj) l
   
