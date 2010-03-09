module Input.GML.ToThijsObject where

import           Shared.Matrix

import qualified Input.GML.Scene   as Scene
import qualified Shared.RenderBase as Thijs

toThijsObject :: Scene.Object -> Thijs.ObjectTree Thijs.Shader
toThijsObject = Scene.foldObject algebra
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

