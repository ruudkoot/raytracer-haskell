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
                    , \o d1 d2 d3    transformation -> o (transformation !*! translate d1 d2 d3)
                    , \o d1 d2 d3    transformation -> o (transformation !*! scale d1 d2 d3)
                    , \o d           transformation -> o (transformation !*! scale d d d)
                    , \o d           transformation -> o (transformation !*! rotateX (radians d))
                    , \o d           transformation -> o (transformation !*! rotateY (radians d))
                    , \o d           transformation -> o (transformation !*! rotateZ (radians d))
                    , \o1 o2         transformation -> Renderer.Union      (o1 transformation) (o2 transformation)
                    , \o1 o2         transformation -> Renderer.Intersect  (o1 transformation) (o2 transformation)
                    , \o1 o2         transformation -> Renderer.Difference (o1 transformation) (o2 transformation)
                    )

toWorld::GML.Scene->Renderer.World
toWorld (GML.Scene amb l obj dp fov w h fil) = 
       Renderer.World (Renderer.RenderOptions (toColour amb) dp fov w h fil) (toRenderObject obj) l
   
