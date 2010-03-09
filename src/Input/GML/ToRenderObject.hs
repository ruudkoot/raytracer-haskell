module Input.GML.ToRenderObject where

import           Data.Matrix
import           Data.Vector
import           Base.Shader
import qualified Input.GML.Scene   as Scene
import qualified Base.Miscellaneous as Render

toRenderObject :: Scene.Object -> Render.ObjectTree Shader
toRenderObject = flip (Scene.foldObject algebra) identity4D
    where algebra = ( \shape shader matrix -> Render.RSimple shape matrix (inverse matrix) shader
                    , \o d1 d2 d3 matrix -> o ((translate d1 d2 d3) !*! matrix)
                    , \o d1 d2 d3 matrix -> o ((diagonal4D (Vector4D (d1, d2, d3, 1))) !*! matrix)
                    , \o d matrix -> o ((diagonal4D (Vector4D (d, d, d, 1))) !*! matrix)
                    , \o d matrix -> o ((rotateX d) !*! matrix)
                    , \o d matrix -> o ((rotateY d) !*! matrix)
                    , \o d matrix -> o ((rotateZ d) !*! matrix)
                    , \o1 o2 matrix -> Render.RUnion      (o1 matrix) (o2 matrix)
                    , \o1 o2 matrix -> Render.RIntersect  (o1 matrix) (o2 matrix)
                    , \o1 o2 matrix -> Render.RDifference (o1 matrix) (o2 matrix)
                    )
          translate a b c = Matrix4D( Vector4D(1, 0, 0, a)
                                    , Vector4D(0, 1, 0, b)
                                    , Vector4D(0, 0, 1, c)
                                    , Vector4D(0, 0, 0, 1))
