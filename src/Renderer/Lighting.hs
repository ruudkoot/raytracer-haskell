-- | Contains the code for applying local lighting. It supports `dynamically'
--   adding of more lighting methods provided the information provided
module Renderer.Lighting (localLighting) where

import Base.Light  
import Base.Shader
  
import Data.Colour
import Data.Vector
  
import Renderer.Intersections2
import Renderer.Scene

  
import Control.Applicative ((<$>))

-- | Applies a list of color transformations using the int. info, the 
--   available lights (TODO: Where do we do occlusion testing??? Should this
--   also contain soft shadows???) for the moment this assumes that the
--   provided lights are the visible lights. :)
--   TODO: Shadows, maybe [(Factor, RenderLight)]  to indicate how heavy a
--         light weighs.
--         Filter out invisible lights.
localLighting :: ColourD -> IntersectionInfo -> [RenderLight] -> SurfaceProperty -> Ray -> ColourD
localLighting (Colour amb) its lights surface r = Colour $ amb * surfC + localC
  where localC = local its surface r lights
        (Colour surfC) = surfaceColour surface


local :: IntersectionInfo -> SurfaceProperty -> Ray -> [RenderLight] -> Vec3D
local isect surface ray = sum . map (\l -> d l + s l) 
  where d l = diffuse isect l surface
        s l = specular isect l surface ray

diffuse :: IntersectionInfo -> RenderLight -> SurfaceProperty -> Vec3D
diffuse isect light surface = case light of 
  (PointLight  pos col) -> let l = lVector isect pos in doLight (n !.! l) col
  (DirectLight dir col) -> doLight (n !.! dir) col 
  _                     -> toVec3D 0 0 0
  where
    n = normal isect
    baseCol = fromColour $ surfaceColour surface 
    dRC     = diffuseReflectionCoefficient surface
    doLight angle col = fmap (angle * dRC *) col * baseCol

lVector isect pos = normalize $ pos - location isect

specular :: IntersectionInfo -> RenderLight -> SurfaceProperty -> Ray -> Vec3D 
specular isect light surface ray = case light of 
  (PointLight pos col) -> let l = lVector isect pos 
                          in doLight (r l !.! v) col
  _                    -> toVec3D 0 0 0 
  where n = normal isect
        angle l = n !.! l
        dir   = dropW $ rDirection ray
        r l    = normalize $ fmap (2 * angle l *) n - l
        v     = normalize $ negate dir
        factor angle = max angle 0 ** phongExponent surface
        sRC = specularReflectionCoefficient surface
        doLight angle = fmap (factor angle * sRC *) 


local' :: ColourD -> IntersectionInfo -> [RenderLight] -> SurfaceProperty -> Ray -> ColourD
local' (Colour amb) its lights surface r = Colour (kdiac + kdsum + kssum + ksisc)
  where kdiac = (kd *) <$> amb * c
        kdsum = (kd *) <$> sum (map (\l -> ((n !.! direction l) *) <$> getIntensity l * c) lights)
        ksisc = (ks *) <$> intensityS * c
        kssum = (ks *) <$> sum (map (\l -> ((n !.! dirhalf l) ** phongExponent surface *) <$> getIntensity l * c) lights)
        kd = diffuseReflectionCoefficient surface
        ks = specularReflectionCoefficient surface
        n = normal its
        (Colour c) = surfaceColour surface
        getIntensity (DirectLight _ i) = i 
        getIntensity (PointLight  _ i) = i
        getIntensity (SpotLight _ _ i _ _) = i
        direction (PointLight pos _) = normalize $ pos - location its
        direction (DirectLight dir _) = normalize $ negate dir
        direction (SpotLight pos _ _ _ _) = normalize $ pos - location its
        dirhalf l = normalize (dropW (rDirection r) `cross` direction l) -- not sure about this
        eye = toVec3D 0 0 (-1)
        intensityS = 1.0 -- No.
                
