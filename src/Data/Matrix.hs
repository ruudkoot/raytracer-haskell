{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
-- | General purpose matrix module.
--

module Data.Matrix where 


import Control.Applicative
import Control.Monad (liftM3, liftM4)
import Test.QuickCheck
import Data.List (intercalate, transpose)
import Data.Vector


import Data.Glome.Vec hiding (translate, scale)
import qualified Data.Glome.Vec as G (translate, scale)

type Matrix4D = Matrix
type Transformation = Xfm


-- Note: changed compared to the original Multiplicable.
class Multiplicable a where 
  (!*!) :: a -> a -> a
  
instance Multiplicable (Transformation) where
  (!*!) = xfm_mult

instance Multiplicable (Matrix4D) where
  (!*!) = mat_mult

-- * Renaming

identityTransformation = ident_xfm

translate x y z = G.translate (vec x y z)
scale x y z = G.scale (vec x y z)

rotateX = rotate (vec 1 0 0)
rotateY = rotate (vec 0 1 0)
rotateZ = rotate (vec 0 0 1)