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

-- type Matrix4D = Matrix
type Transformation = Xfm

(!*!) = xfm_mult
multiplyTransformations t1 t2 = xfm_mult t2 t1

-- * Renaming

identityTransformation = ident_xfm

translate x y z = G.translate (vec x y z)
scale x y z = G.scale (vec x y z)

rotateX = rotate (vec 1 0 0)
rotateY = rotate (vec 0 1 0)
rotateZ = rotate (vec 0 0 1)