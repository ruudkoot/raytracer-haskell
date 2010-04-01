{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
-- | General purpose matrix module.
--

module Data.Matrix where 


import Control.Applicative
import Control.Monad (liftM3, liftM4)

import Data.Angle
import Data.List (intercalate, transpose)
import Data.Vector

import Test.QuickCheck


import Data.Glome.Vec hiding (translate, scale)
import qualified Data.Glome.Vec as G (translate, scale)

-- type Matrix4D = Matrix
type Transformation = Xfm

(!*!) = xfm_mult
multiplyTransformations t1 t2 = xfm_mult t2 t1

transformPoint = xfm_point
transformVector = xfm_vec

-- * Renaming

identityTransformation = ident_xfm

translate x y z = G.translate (vec x y z)
scale x y z = G.scale (vec x y z)

rotateX r = rotate (vec 1 0 0) (radians r)
rotateY r = rotate (vec 0 1 0) (radians r)
rotateZ r = rotate (vec 0 0 1) (radians r)
