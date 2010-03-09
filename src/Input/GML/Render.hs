module Input.GML.Render where
  
import Shared.Vector
import Shared.RenderBase

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