module Renderer.Datatypes where
  
import qualified Shared.Colour (Colour)
import qualified Shared.Vector (Point3D, Vector3D)

type Point3D = Shared.Vector.Point3D Double
type Vector3D = Shared.Vector.Vector3D Double

type Colour = Shared.Colour.Colour Int


-- | The global datatype, also referenced to as `scene'. We pushed down some
-- of the parameters as stated in the render function of gml for ease.
data World surface = World 
  {
    options :: RenderOptions
  , object  :: RenderObject surface
  , lights  :: [RenderLight]
  }
  
-- | Type inference causes restriction `Shader a` => on the a
data RenderObject a = Sphere a
                  | Cube a
                  | Cylinder a
                  | Cone a
                  | Plane a
                  | Translate (RenderObject a) Int Int Int
                  | Scale (RenderObject a) Int Int Int
                  | UScale (RenderObject a) Int
                  | RotateX (RenderObject a) Int
                  | RotateY (RenderObject a) Int
                  | RotateZ (RenderObject a) Int
                  | Union (RenderObject a) (RenderObject a)
                  | Intersect (RenderObject a) (RenderObject a)
                  | Difference (RenderObject a) (RenderObject a) 
                  
data RenderLight = RenderLight 
  {
    position  :: Point3D
  , intensity :: Point3D
  }
                  

-- | Our |Shader| mechanism.
-- Example: 
-- instance Shader GML where
--   shade surface ray = eval surface ray       
class Shader s where
  shade :: s -> Ray -> a


data RenderOptions = RenderOptions
  {
    ambience :: Colour -- amb
  , depth    :: Int
  , fov      :: Double -- fov
  , width    :: Int -- wid
  , height   :: Int -- ht
  , file     :: FilePath
  }
  

data Ray = Ray 
  {
    origin    :: Vector3D
  , direction :: Vector3D
  }