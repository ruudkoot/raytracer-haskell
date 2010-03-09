module Input.GML.Render where
  
import Shared.Vector
import Shared.RenderBase

data Object = Simple     Shape  Shader
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
          
data Render = Render
  { gmlAmbience :: Pt3D
  , gmlLights   :: [RenderLight]
  , gmlObj      :: Object
  , gmlDepth    :: Int
  , gmlFov      :: Double        -- fov
  , gmlWidth    :: Int           -- wid
  , gmlHeight   :: Int           -- ht
  , gmlFile     :: FilePath
  }
  deriving (Show,Eq)

type ObjectAlgebra r =
    ( Shape -> Shader -> r
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
