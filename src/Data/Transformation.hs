{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
-- | Interface to the glomevec transformations, intended to be (mostly)
--   compatible with the matrix module we used in the first place.
--

module Data.Transformation where 

import Data.Vector
import Data.Angle

import Test.QuickCheck

import Data.Glome.Vec hiding (translate, scale)
import qualified Data.Glome.Vec as G (translate, scale)

type Transformation = Xfm

-- Using the glomevec transformations

(!*!) = xfm_mult

transformPoint = xfm_point

transformVector = xfm_vec

transformVector' = invxfm_vec

identityTransformation = ident_xfm

-- Standard transformations

translate x y z = G.translate (vec x y z)
scale x y z = G.scale (vec x y z)

rotateX r = rotate (vec 1 0 0) (radians r)
rotateY r = rotate (vec 0 1 0) (radians r)
rotateZ r = rotate (vec 0 0 1) (radians r)
