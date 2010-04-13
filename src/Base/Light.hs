module Base.Light where

import Data.Vector

data RenderLight = 

  -- | Light source at pos infinity.
  -- 
    DirectLight { dlDirection  :: Pt3D 
                , dlColor      :: Pt3D }

  -- | Point light source
  --
  | PointLight { plPosition :: Pt3D
               , plColor    :: Pt3D }

  -- | Spotlight with cutoff
  --
  | SpotLight { slPosition    :: Pt3D
              , slTarget      :: Pt3D
              , slColor       :: Pt3D
              , slCutoff      :: Double
              , slAttenuation :: Double }        

  deriving (Show,Eq)  

