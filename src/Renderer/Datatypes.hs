module Renderer.Datatypes where
  
import qualified Shared.Colour (Colour)
import qualified Shared.Vector (Vector3D)
import Shared.Matrix (Matrix3D)

type Point3D = Shared.Vector.Vector3D Double
type Vector3D = Shared.Vector.Vector3D Double

type Colour = Shared.Colour.Colour Double

newtype Degrees     = Degrees Double
newtype Attenuation = Attenuation Double


-- | The global datatype, also referenced to as `scene'. We pushed down some
-- of the parameters as stated in the render function of gml for ease. We are
-- not entirely sure whether the `union' representation of the objects is 
-- workable for us.
data World surface = World 
  {
    wOptions :: RenderOptions
  , wObject  :: RenderObject surface
  , wLights  :: [RenderLight]
  }
  
-- | Type inference causes restriction 'Shader' a => on the a
data GMLObject a = Simple Shape
                 | Translate (GMLObject a) Int Int Int
                 | Scale (GMLObject a) Int Int Int
                 | UScale (GMLObject a) Int
                 | RotateX (GMLObject a) Int
                 | RotateY (GMLObject a) Int
                 | RotateZ (GMLObject a) Int
                 | Union (GMLObject a) (GMLObject a)
                 | Intersect (GMLObject a) (GMLObject a)
                 | Difference (GMLObject a) (GMLObject a) 
          
                  
data RenderLight
  -- | Light source at pos infinity.
  = DirectLight 
  {
    dlDirection  :: Point3D
  , dlColor      :: Point3D
  }
  -- | Point light source
  | PointLight 
  {
    plPosition :: Point3D
  , plColor    :: Point3D
  }
  -- | Spotlight with cutoff
  | SpotLight 
  {
    slPosition    :: Point3D
  , slTarget      :: Point3D
  , slCutoff      :: Double -- Degrees
  , slAttenuation :: Double -- ???
  }        
    
  -- dir color  light  l
  --  creates a directional light source at infinity with direction dir and intensity color. Both dir and color are specified as point values.
  --  pos color  pointlight  l
  --  creates a point-light source at the world coordinate position pos with intensity color. Both pos and color are specified as point values. Pointlights are a Tier-2 feature.
  -- 
  --  pos at color cutoff exp  spotlight  l
  --  creates a spotlight source at the world coordinate position pos pointing
  --  towards the position at. The light's color is given by color. The
  --  spotlight's cutoff angle is given in degrees by cutoff and the attenuation
  --  exponent is given by exp (these are real numbers). The intensity of the
  --  light from a spotlight at a point Q is determined by the angle between the
  --  light's direction vector (i.e., the vector from pos to at) and the vector
  --  from pos to Q. If the angle is greater than the cutoff angle, then intensity
  --  is zero; otherwise the intensity is given by the equation

-- | Our 'Shader' mechanism.
-- Example: 
-- instance Shader GML where
--   shade surface ray = eval surface ray       
class Shader s where
  shade :: s -> Ray -> a


data RenderOptions = RenderOptions
  {
    roAmbience :: Colour -- amb
  , roDepth    :: Int
  , roFov      :: Double -- fov
  , roWidth    :: Int -- wid
  , roHeight   :: Int -- ht
  , roFile     :: FilePath
  }
  

data Ray = Ray 
  {
    rOrigin    :: Vector3D
  , rDirection :: Vector3D
  }

data Shape = Cube | Cylinder | Sphere | Cone | Plane

data RenderObject a = RenderObject
  {
    moShape          :: Shape
  , moSurface        :: a
  , moTransformation :: Matrix3D Double -- Should that be Matrix4D?
  , moIntersections  :: [ObjectTree a]
  , moDifferences    :: [ObjectTree a]
  }
  
data ObjectTree a = RSimple Shape (Matrix3D Double) -- Should indeed be Matrix4D
                  | RUnion (ObjectTree a) (ObjectTree a)
                  | RIntersect (ObjectTree a) (ObjectTree a)
                  | RDifference (ObjectTree a) (ObjectTree a)
