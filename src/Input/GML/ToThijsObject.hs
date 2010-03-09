module Input.GML.ToThijsObject where

import           Shared.Matrix
import qualified Input.GML.Render  as Render
import qualified Shared.RenderBase as Thijs

toThijsObject :: Render.Object -> Thijs.ObjectTree Thijs.Shader
toThijsObject = flip (Render.foldObject algebra) identity4D
    where algebra = ( \shape shader matrix -> Thijs.RSimple shape matrix matrix shader
                    , undefined
                    , undefined
                    , undefined
                    , undefined
                    , undefined
                    , undefined
                    , \o1 o2 matrix -> Thijs.RUnion      (o1 matrix) (o2 matrix)
                    , \o1 o2 matrix -> Thijs.RIntersect  (o1 matrix) (o2 matrix)
                    , \o1 o2 matrix -> Thijs.RDifference (o1 matrix) (o2 matrix)
                    )

