module Base.Shader where

import Data.Colour

-- ** Shaders
type ShaderCoord = (Double,Double,Int)
data ShaderResult = ShaderResult
    {
     srColor::ColourD
    ,srKd::Double
    ,srKs::Double
    ,srPhong::Double
    }

newtype Shader = Shader (ShaderCoord -> ShaderResult)
instance Show Shader where
    show s = "Shader function"
instance Eq Shader where
    (==) a b = error "Alessandro" --True

