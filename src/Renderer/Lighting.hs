-- | Contains the code for applying local lighting. It supports `dynamically'
--   adding of more lighting methods provided the information provided <-- what?
module Renderer.Lighting (localLighting) where

import Base.Light (RenderLight(..))
import Base.Shader (SurfaceProperty(..))
  
import Data.Colour (Colour, fromColour)
import Data.Vector 
import Data.Radians

import Renderer.Intersections
import Renderer.IntersectionInfo

import Control.Applicative ((<$>))
import Data.Maybe
import Renderer.Scene 


-- Ruud's attempt at the illumination model

d ~> f = f d
r .* v = (r*) `vmap` v

illumination :: World -> Ray -> IntersectionInfo -> SurfaceProperty -> Vec3D
illumination world ray intersectionInfo surfaceProperty =
    let c'  = fromColour $ surfaceProperty  ~> surfaceColour  
        i_a = fromColour $ world            ~> (roAmbience . wOptions)
        k_d =              surfaceProperty  ~> diffuseReflectionCoefficient
        n'  =              intersectionInfo ~> normal
        k_s =              surfaceProperty  ~> specularReflectionCoefficient 
        n   =              surfaceProperty  ~> phongExponent

        loc              = location intersectionInfo
        unshadowedLights = filter (not . shadowed (location intersectionInfo) (wObject world)) $ wLights world

        ambient    = k_d .* (i_a * c')
        diffuse    = k_d .* sum [(n' !.! l'_j)      .* (i'_j * c') | (l'_j, i'_j) <- j]
                      where j = map (\l -> (direction loc l, getIntensity l loc)) unshadowedLights
        specular   = k_s .* sum [((n' !.! h'_j) ** n) .* (i'_j * c') | (h'_j, i'_j) <- j]
                      where j = map (\l -> (normalize $ (normalize (negate (rDirection ray)) + normalize (direction loc l)), getIntensity l loc)) unshadowedLights
        reflection = undefined
        
     in ambient + clamp diffuse + specular {- + reflection -}
        where clamp = vmap (max 0.0)


-- Calculate the local lighting.
-- This basically implements the lighting model 
-- from page 11 of the assigment.
--
localLighting :: IntersectionInfo -> World -> SurfaceProperty -> Ray -> Vec3D -> Vec3D
localLighting its world surface ray reflected = illumination world ray its surface

{-
localLighting its world surface r reflected = diffuse + specular
  where ambient    = fromColour . roAmbience $ wOptions world        
        diffuse    = col (diffuseReflectionCoefficient surface) ambient dirLight lights
        specular   = col (specularReflectionCoefficient surface) reflected phong lights
        col k i f l= (k*) `vmap` (i * surfC + sum (map f l))

        dirLight l = light (n !.! dir l) l 
        phong    l = light ((n !.! dirhalf l) ** phongExponent surface) l
        light  f l = (max 0.0) `vmap` ((f*) `vmap` (getIntensity l (location its) * surfC))

        dirhalf  l = normalize $ (normalize (negate (rDirection r)) + normalize (dir l))
        dir        = direction (location its)
        
        surfC      = fromColour $ surfaceColour surface
        n          = normal its
        lights     = wLights world 
        lightsv    = filter (not . shadowed (location its) (wObject world)) lights
-}

-- | Get the unit vector from a location 
-- to a RenderLight's position.
--
direction :: Pt3D -> RenderLight -> Vec3D
direction loc (SpotLight   pos _ _ _ _ ) = normalize $ pos - loc
direction loc (PointLight  pos _       ) = normalize $ pos - loc
direction _   (DirectLight dir _       ) = normalize $ negate dir


-- | H vector
--
halfWay :: Pt3D -> RenderLight -> Vec3D
halfWay loc (SpotLight   pos _ _ _ _ ) = normalize $ pos - loc
halfWay loc (PointLight  pos _       ) = normalize $ pos - loc
halfWay _   (DirectLight dir _       ) = normalize $ negate dir

-- | Calculate the intensity of RenderLight 
-- at a certain position. 
--
getIntensity :: RenderLight -> Pt3D -> Vec3D               
getIntensity (DirectLight _  i) _ = i 
getIntensity (PointLight pos i) loc = attenuate (magnitude (loc - pos)) i
getIntensity (SpotLight pos at i cutoff exp) loc = attenuate (magnitude (loc - pos)) i'
  where i' = if angle > radians cutoff then toVec3D 0 0 0 else spot
        spot = ((dir !.! posDir) ** exp *) `vmap` i
        dir = normalize $ at - pos 
        posDir = normalize $ loc - pos
        {-spot = (((dir / abs dir) !.! (posDir / abs posDir)) ** exp *) `vmap` i
        dir = at - pos
        posDir = loc - pos-}
        angle = acos(dir !.! posDir)


-- | The light from point lights and spotlights is 
-- attenuated by the distance from the light to 
-- the surface.
-- 
attenuate :: Double -> Vec3D -> Vec3D
attenuate d = vmap ((/ dis) . (100*))
  where dis = 99 + d ** 2


shadowed :: Vector3D -> Object -> RenderLight -> Bool
shadowed p o (DirectLight l _)     = not.null . intersect (mkShadowRay p (negate l)) $ o
shadowed p o (PointLight l _)      = hit (mkShadowRay p l) o
shadowed p o (SpotLight l _ _ _ _) = hit (mkShadowRay p l) o

mkShadowRay :: Vector3D -> Vector3D -> Ray
mkShadowRay p l = let direction = l - p
                      p' = p + (0.01 * direction)
                  in mkRay p' direction


