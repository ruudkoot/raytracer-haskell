module Input.GML.ToRenderObject where

import           Data.Matrix
import           Data.Vector
import           Data.Colour

import qualified Input.GML.Scene as GML
import qualified Renderer.Scene  as Renderer

toRenderObject :: GML.Object -> Renderer.Object
toRenderObject = flip (GML.foldObject algebra) identity4D
    where algebra = ( \shape shader matrix -> Renderer.Simple shape matrix (inverse matrix) shader
                    , \o d1 d2 d3   matrix -> o (translate d1 d2 d3 !*! matrix)
                    , \o d1 d2 d3   matrix -> o (diagonal4D (Vector4D (d1, d2, d3, 1)) !*! matrix)
                    , \o d          matrix -> o (diagonal4D (Vector4D (d, d, d, 1)) !*! matrix)
                    , \o d          matrix -> o (rotateX d !*! matrix)
                    , \o d          matrix -> o (rotateY d !*! matrix)
                    , \o d          matrix -> o (rotateZ d !*! matrix)
                    , \o1 o2        matrix -> Renderer.Union      (o1 matrix) (o2 matrix)
                    , \o1 o2        matrix -> Renderer.Intersect  (o1 matrix) (o2 matrix)
                    , \o1 o2        matrix -> Renderer.Difference (o1 matrix) (o2 matrix)
                    )

toWorld::GML.Scene->Renderer.World
toWorld (GML.Scene (Vector3D amb) l obj dp fov w h fil) = 
       Renderer.World (Renderer.RenderOptions (Colour amb) dp fov w h fil) (toRenderObject obj) l
   
