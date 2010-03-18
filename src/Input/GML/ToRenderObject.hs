module Input.GML.ToRenderObject where

import           Data.Matrix
import           Data.Vector
import           Data.Colour
import           Data.Radians

import qualified Input.GML.AST      as GML
import qualified Input.GML.Evaluate as Evil
import qualified Renderer.Scene     as Renderer

toRenderObject :: GML.Object -> Renderer.Object
toRenderObject = flip (GML.foldObject algebra) identity4D
    where algebra = ( \shape closure matrix -> Renderer.Simple shape matrix (inverse matrix) (Evil.shader closure)
                    , \o d1 d2 d3    matrix -> o (matrix !*! translate d1 d2 d3)
                    , \o d1 d2 d3    matrix -> o (matrix !*! diagonal4D (Vector4D (d1, d2, d3, 1)))
                    , \o d           matrix -> o (matrix !*! diagonal4D (Vector4D (d, d, d, 1)))
                    , \o d           matrix -> o (matrix !*! rotateX (radians d))
                    , \o d           matrix -> o (matrix !*! rotateY (radians d))
                    , \o d           matrix -> o (matrix !*! rotateZ (radians d))
                    , \o1 o2         matrix -> Renderer.Union      (o1 matrix) (o2 matrix)
                    , \o1 o2         matrix -> Renderer.Intersect  (o1 matrix) (o2 matrix)
                    , \o1 o2         matrix -> Renderer.Difference (o1 matrix) (o2 matrix)
                    )

toWorld::GML.Scene->Renderer.World
toWorld (GML.Scene amb l obj dp fov w h fil) = 
       Renderer.World (Renderer.RenderOptions (Colour amb) dp fov w h fil) (toRenderObject obj) l
   
