module Renderer.Datatypes where
  
import qualified Shared.Colour (Colour)
import qualified Shared.Vector (Vector4D, Vector3D)
import Shared.Matrix (Matrix4D)

type Pt3D  = Shared.Vector.Vector3D Double
type Vec4D = Shared.Vector.Vector4D Double

type Colour = Shared.Colour.Colour Double

--newtype Degrees     = Degrees Double deriving (Show,Eq)
--newtype Attenuation = Attenuation Double deriving (Show,Eq)

type ShaderCoord = (Double,Double,Int)
data ShaderResult = ShaderResult
    {
     srColor::Colour
    ,srKd::Double
    ,srKs::Double
    ,srPhong::Double
    }

newtype Shader = Shader (ShaderCoord -> ShaderResult)
instance Show Shader where
    show s = "Shader function"
instance Eq Shader where
    (==) a b = True

-- Maybe these should be functions from <u,v> to colour and not ray to colour?


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
data GMLObject   = Simple Shape Shader
                 | Translate GMLObject  Double Double Double
                 | Scale GMLObject Double Double Double
                 | UScale GMLObject Double 
                 | RotateX GMLObject Double
                 | RotateY GMLObject Double
                 | RotateZ GMLObject Double
                 | Union GMLObject GMLObject
                 | Intersect GMLObject GMLObject
                 | Difference GMLObject GMLObject
                 deriving (Show,Eq)
          
                  
data RenderLight
  -- | Light source at pos infinity.
  = DirectLight 
  {
    dlDirection  :: Pt3D
  , dlColor      :: Pt3D
  }
  -- | Point light source
  | PointLight 
  {
    plPosition :: Pt3D
  , plColor    :: Pt3D
  }
  -- | Spotlight with cutoff
  | SpotLight 
  {
    slPosition    :: Pt3D
  , slTarget      :: Pt3D
  , slColor       :: Pt3D
  , slCutoff      :: Double
  , slAttenuation :: Double
  }        
  deriving (Show,Eq)  
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

data GMLRender = GMLRender
  {
    gmlAmbience :: Pt3D
  , gmlLights   :: [RenderLight]
  , gmlObj      :: GMLObject
  , gmlDepth    :: Int
  , gmlFov      :: Double -- fov
  , gmlWidth    :: Int -- wid
  , gmlHeight   :: Int -- ht
  , gmlFile     :: FilePath
  }
  deriving (Show,Eq)

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
    rOrigin    :: Vec4D
  , rDirection :: Vec4D
  }

data Shape = Cube | Cylinder | Sphere | Cone | Plane deriving (Show,Eq)

data RenderObject a = RenderObject
  {
    moShape          :: Shape
  , moSurface        :: Shader
  , moTransformation :: Matrix4D Double
  , moIntersections  :: [ObjectTree a]
  , moDifferences    :: [ObjectTree a]
  }
  
data ObjectTree a = RSimple Shape (Matrix4D Double)
                  | RUnion (ObjectTree a) (ObjectTree a)
                  | RIntersect (ObjectTree a) (ObjectTree a)
                  | RDifference (ObjectTree a) (ObjectTree a)
