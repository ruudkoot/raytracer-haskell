module Renderer.Renderer (renderFile) where

import Base.CLI      (ProgramOptions(..))
import Base.Shader   (runShader)

import Data.Angle
import Data.Colour   (Colour(..), Colours, toRGB, toColour)
import Data.Vector   (toVec3D, (!.!), Ray, mkRay, rDirection, vector3D, vmap, Pt3D, Vec3D)

import Input.GML.RunGML (runGML, toWorld)

import Output.PPM    (toPPM)

import Renderer.IntersectionInfo (IntersectionInfo(..),nearest)
import Renderer.Intersections    (intersect)
import Renderer.Lighting         (localLighting)
import Renderer.Scene            (World(..), RenderOptions(..), getDimensions)

import Control.Parallel.Strategies


-- | A RayMaker produces a Ray when given 
-- its pixel coordinates.
--
type RayMaker = Int -> Int -> Int -> Int -> Ray



-- | Render a single file. 
--
renderFile :: ProgramOptions -> FilePath -> IO ()
renderFile cargs fp = do putStrLn $ "Rendering file: " ++ fp 
                         (scenes, textures) <- runGML fp
                         mapM_ (renderScene cargs . toWorld textures) scenes          


-- | Renders the World.
--
renderScene :: ProgramOptions -> World -> IO ()
renderScene poptions world = pixels `seq` saveRendering world pixels
  where raymaker = getRayMaker world poptions
        (w,h)    = getDimensions world
        depth    = (roDepth.wOptions) world
        renderer = renderPixel depth (aa poptions) raymaker world
        pixels   = [renderer i (h-j)  |  j <- [0..h-1], i <- [0..w-1]]
                     `using` parListChunk w rdeepseq


-- | Calculates the colour for a single pixel position 
-- by recursively shooting a ray into the World.
--
renderPixel :: Int -> Int -> RayMaker -> World -> Int -> Int -> Colour Int
renderPixel dep aa raymaker world x y = toRGB . toColour . vmap ( / aasquared) . sum $! 
                                       map (\ (aai, aaj) -> renderPixel' dep (aaraymaker aai aaj) id)
                                           [(i,j) | i <- [1..aa], j <- [1..aa]]
  where 
    aasquared  = fromIntegral $ aa*aa
    aaraymaker = raymaker x y
    renderPixel' depth ray k = 
      case nearest $ intersect ray (wObject world) of 
        Nothing     -> k $! toVec3D 0 0 0 -- No intersections. Intensity=0
        (Just info) -> if depth == 0 then k $! toVec3D 0 0 0
                       else let surface   = runShader (shader info) $! textureCoord info
                                reflected = reflectedRay (location info) (rDirection ray) (normal info)
                            in renderPixel' (depth - 1) reflected $! (k . localLighting info world surface ray)                 
                    
reflectedRay :: Pt3D -> Vec3D -> Vec3D -> Ray 
reflectedRay origin rdirection norm = mkRay clearasil direction
  where 
    reflDir = vmap (2 * norm !.! rdirection *) norm
    direction = rdirection - reflDir 
    clearasil = origin + 0.01 * direction 


-- | Saves the calculated colours to a PPM file (the 
-- location of which is specified in the GML)
--
saveRendering :: World -> Colours Int -> IO ()
saveRendering world pix = maybe bad save $! toPPM w h pix
  where bad    = error "Error: didn't produce a valid PPM image."
        (w,h)  = getDimensions world
        save p = do putStrLn ("writing result to " ++ roFile (wOptions world)) ;
		            writeFile (roFile (wOptions world)) p


-- | Creates the RayMaker for the given world.
--
getRayMaker :: World -> ProgramOptions -> RayMaker 
getRayMaker world options = mkRayMaker (aa options)  x y dx dy
  where (w,h)   = getDimensions world
        (x,y)   = (-tan(0.5 * radians fov), x * fromIntegral h / fromIntegral w)
        (dx,dy) = (-2 * x / fromIntegral w, -2 * y / fromIntegral h)
        fov     = roFov (wOptions world)


-- | A RayMakerMaker, if you will; but you won't.
--
mkRayMaker :: Int -> Double -> Double -> Double-> Double -> RayMaker 
mkRayMaker aa x y dx dy i j aai aaj = mkRay eye dir
  where aadx = dx / fromIntegral aa
        aady = dy / fromIntegral aa
        eye  = vector3D (0, 0, -1)
        dir  = vector3D ( x + fromIntegral i * dx + 0.5 * aadx + fromIntegral aai * aadx
                        , y + fromIntegral j * dy + 0.5 * aady + fromIntegral aaj * aady
                        , 1
                        )
