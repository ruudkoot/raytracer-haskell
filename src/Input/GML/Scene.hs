module Input.GML.Scene where
  
import Shared.Vector

import qualified Base.Light  as Light
import qualified Base.Shader as Shader
import qualified Base.Shape  as Shape

data Scene = Scene
  { sceneAmbience :: Pt3D
  , sceneLights   :: [Light.RenderLight]
  , sceneObj      :: Object
  , sceneDepth    :: Int
  , sceneFov      :: Double        -- fov
  , sceneWidth    :: Int           -- wid
  , sceneHeight   :: Int           -- ht
  , sceneFile     :: FilePath
  }
  deriving (Show,Eq)

data Object = Simple     Shape.Shape  Shader.Shader
            | Translate  Object Double Double Double
            | Scale      Object Double Double Double
            | UScale     Object Double 
            | RotateX    Object Double
            | RotateY    Object Double
            | RotateZ    Object Double
            | Union      Object Object
            | Intersect  Object Object
            | Difference Object Object
            deriving (Show,Eq)
          
type ObjectAlgebra r =
    ( Shape.Shape -> Shader.Shader -> r
    , r -> Double -> Double -> Double -> r
    , r -> Double -> Double -> Double -> r
    , r -> Double -> r
    , r -> Double -> r
    , r -> Double -> r
    , r -> Double -> r
    , r -> r -> r
    , r -> r -> r
    , r -> r -> r
    )

foldObject :: ObjectAlgebra r -> Object -> r
foldObject (simple, translate, scale, uscale, rotatex, rotatey, rotatez, union,
              intersect, difference) = fold
    where fold x = case x of {
        Simple     shape   shader   -> simple     shape          shader;
        Translate  object  d1 d2 d3 -> translate  (fold object ) d1 d2 d3;
        Scale      object  d1 d2 d3 -> scale      (fold object ) d1 d2 d3;
        UScale     object  double   -> uscale     (fold object ) double;
        RotateX    object  double   -> rotatex    (fold object ) double;
        RotateY    object  double   -> rotatey    (fold object ) double;
        RotateZ    object  double   -> rotatez    (fold object ) double;
        Union      object1 object2  -> union      (fold object1) (fold object2);
        Intersect  object1 object2  -> intersect  (fold object1) (fold object2);
        Difference object1 object2  -> difference (fold object1) (fold object2);
      }
