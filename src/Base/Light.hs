module Base.Light where

import Data.Vector

-- | RenderLight is used by both represantations, GML and OUrs                  
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

getColor :: RenderLight -> Pt3D
getColor (DirectLight _ c) = c
getColor (PointLight  _ c) = c
getColor (SpotLight   _ _ c _ _) = c 