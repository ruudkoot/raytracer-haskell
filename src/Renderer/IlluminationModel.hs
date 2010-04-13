-- | Contains the code for applying local lighting. It supports `dynamically'
--   adding of more lighting methods provided the information provided <-- what?
module Renderer.IlluminationModel (localLighting) where

import Base.Light (Light(..))
import Base.Shader (SurfaceProperty(..))

import Data.Angle
import Data.Colour (fromColour)
import Data.Vector 

import Renderer.Intersections
import Renderer.IntersectionInfo

import Renderer.Scene 


-- Calculate the local lighting.
-- This basically implements the lighting model 
-- from page 11 of the assigment.
--
localLighting :: IntersectionInfo -> Scene -> SurfaceProperty -> Ray -> Vec3D -> Vec3D
localLighting its world surface r reflected = surfC * diffuse + specular
  where ambient    = fromColour . ambience $ options world        
        diffuse    = col (diffuseReflectionCoefficient surface) ambient dirLight lightsv
        specular   = col (specularReflectionCoefficient surface) (surfC * reflected) phong lightsv
        col k i f l= (k*) `vmap` (i + sum (map (clamp . f) l))
        
        clamp = vmap (\i -> max (min i 1.0) 0.0)

        dirLight l = ((n !.! dir l)*) `vmap` getIntensity l (location its)
        phong    l = (((n !.! dirhalf l) ** phongExponent surface)*) `vmap` getIntensity l (location its)

        dirhalf  l = normalize (normalize (negate (rDirection r)) + dir l)
        dir        = normalize . direction (location its)
        
        surfC      = fromColour $ surfaceColour surface
        n          = normal its
        lightsv    = filter (not . shadowed (location its) (object world) n) (lights world)


-- | Get the unit vector from a location 
-- to a RenderLight's position.
--
direction :: Pt3D -> Light -> Vec3D
direction loc (SpotLight   pos _ _ _ _ ) = normalize $ pos - loc
direction loc (PointLight  pos _       ) = normalize $ pos - loc
direction _   (DirectLight dir _       ) = normalize $ negate dir


-- | Calculate the intensity of RenderLight 
-- at a certain position. 
--
getIntensity :: Light -> Pt3D -> Vec3D               
getIntensity (DirectLight _  i) _ = i 
getIntensity (PointLight pos i) loc = attenuate (magnitude (loc - pos)) i
getIntensity (SpotLight pos at i cutoff xp) loc = attenuate (magnitude (loc - pos)) i'
  where i' = if Radians angle > toRadians (Degrees cutoff) then toVec3D 0 0 0 else spot
        spot = ((dir !.! posDir) ** xp *) `vmap` i
        dir = normalize $ at - pos 
        posDir = normalize $ loc - pos
        angle = acos(dir !.! posDir)


-- | The light from point lights and spotlights is 
-- attenuated by the distance from the light to 
-- the surface.
-- 
attenuate :: Double -> Vec3D -> Vec3D
attenuate d = vmap ((/ dis) . (100*))
  where dis = 99 + d ** 2

-- | Shadow feeler function. Calls on hit or intersect to determine whether there is an object within the shadow ray.
shadowed :: Vector3D -> Object -> Vector3D -> Light -> Bool
shadowed p o n (DirectLight l _)     = not . null . intersect (mkShadowRay p (negate l) n) $ o
shadowed p o n (PointLight l _)      = hit (mkShadowRay p (l-p) n) o
shadowed p o n (SpotLight l _ _ _ _) = hit (mkShadowRay p (l-p) n) o

mkShadowRay :: Vector3D -> Vector3D -> Vector3D -> Ray
mkShadowRay p d n = let p' = p + (0.001 * n)
                    in mkRay p' d


