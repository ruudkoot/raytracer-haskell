module Renderer.Datatypes where
  
type Point = (Real, Real, Real)
  
data World = World RenderOptions
                   RenderObject
                   [RenderLights]
                   -- AmbientLight
    
-- Type inference causes restriction Surface a => on the a
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
                  
                  
class Surface b where
  shade :: -> a

data RenderOptions = RenderOptions
  {
    amb   :: Point
    depth :: Int
    fov   :: Double
    wid   :: Int
    ht    :: Int
    file  :: FilePath
  }