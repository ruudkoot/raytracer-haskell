module Input.GML.ToThijsObject where

import           Shared.Matrix
import           Shared.Vector
import qualified Input.GML.Scene   as Scene
import qualified Shared.RenderBase as Thijs

toThijsObject :: Scene.Object -> Thijs.ObjectTree Thijs.Shader
toThijsObject = flip (Scene.foldObject algebra) identity4D
    where algebra = ( \shape shader matrix -> Thijs.RSimple shape matrix (inverse matrix) shader
                    , \o d1 d2 d3 matrix -> o ((translate d1 d2 d3) !*! matrix)
                    , \o d1 d2 d3 matrix -> o ((diagonal4D (Vector4D (d1, d2, d3, 1))) !*! matrix)
                    , \o d matrix -> o ((scaleF d identity4D) !*! matrix)
                    , \o d matrix -> o ((rotatex d) !*! matrix)
                    , \o d matrix -> o ((rotatey d) !*! matrix)
                    , \o d matrix -> o ((rotatez d) !*! matrix)
                    , \o1 o2 matrix -> Thijs.RUnion      (o1 matrix) (o2 matrix)
                    , \o1 o2 matrix -> Thijs.RIntersect  (o1 matrix) (o2 matrix)
                    , \o1 o2 matrix -> Thijs.RDifference (o1 matrix) (o2 matrix)
                    )
          translate a b c = Matrix4D( Vector4D(1, 0, 0, a)
                                    , Vector4D(0, 1, 0, b)
                                    , Vector4D(0, 0, 1, c)
                                    , Vector4D(0, 0, 0, 1))
          rotatex d = Matrix4D( Vector4D(1, 0, 0, 0)
                              , Vector4D(0, cos d, -sin d, 0)
                              , Vector4D(0, sin d, cos d, 0)
                              , Vector4D(0, 0, 0, 1))
          rotatey d = Matrix4D( Vector4D(cos d, 0, -sin d, 0)
                              , Vector4D(0, 1, 0, 0)
                              , Vector4D(sin d, 0, cos d, 0)
                              , Vector4D(0, 0, 0, 1))
          rotatez d = Matrix4D( Vector4D(cos d, -sin d, 0, 0)
                              , Vector4D(sin d, cos d, 0, 0)
                              , Vector4D(0, 0, 1, 0)
                              , Vector4D(0, 0, 0, 1))