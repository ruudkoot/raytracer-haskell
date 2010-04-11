module Input.GML.ToRenderObject where

import           Data.Angle
import           Data.Colour
import           Data.Transformation
import           Data.Texture

import qualified Input.GML.AST      as GML
import qualified Input.GML.Evaluate as Evil
import qualified Renderer.Scene     as Renderer

toRenderObject :: Textures -> GML.Object -> Renderer.Object
toRenderObject txs = flip (GML.foldObject algebra) identityTransformation
    where algebra = ( GML.SimpleTransformer $
                      \shape closure transformation -> Renderer.Simple shape transformation (Evil.shader txs closure)
                    , \o d1 d2 d3    transformation -> o (transformation !*! translate d1 d2 d3)
                    , \o d1 d2 d3    transformation -> o (transformation !*! scale d1 d2 d3)
                    , \o d           transformation -> o (transformation !*! scale d d d)
                    , \o d           transformation -> o (transformation !*! rotateX (toRadians d))
                    , \o d           transformation -> o (transformation !*! rotateY (toRadians d))
                    , \o d           transformation -> o (transformation !*! rotateZ (toRadians d))
                    , \o1 o2         transformation -> Renderer.Union      (o1 transformation) (o2 transformation)
                    , \o1 o2         transformation -> Renderer.Intersect  (o1 transformation) (o2 transformation)
                    , \o1 o2         transformation -> Renderer.Difference (o1 transformation) (o2 transformation)
                    )
                    
toWorld :: Textures -> GML.Scene -> Renderer.World
toWorld txs (GML.Scene amb l obj dp fov w h fil) = 
       Renderer.World (Renderer.RenderOptions (toColour amb) dp (toRadians fov) w h fil) (toRenderObject txs obj) l

