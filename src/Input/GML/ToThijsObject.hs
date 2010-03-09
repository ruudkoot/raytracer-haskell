module Input.GML.ToThijsObject where

import           Shared.Matrix
import qualified Input.GML.Render  as Render
import qualified Shared.RenderBase as Thijs

toThijsObject :: Render.Object -> Thijs.ObjectTree Thijs.Shader
toThijsObject = Render.foldObject algebra
    where algebra = ( \shape shader -> Thijs.RSimple shape identity4D identity4D shader
                    , undefined
                    , undefined
                    , undefined
                    , undefined
                    , undefined
                    , undefined
                    , \o1 o2 -> Thijs.RUnion      o1 o2
                    , \o1 o2 -> Thijs.RIntersect  o1 o2
                    , \o1 o2 -> Thijs.RDifference o1 o2
                    )

