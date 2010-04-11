{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
-- | Interface to the glomevec transformations, intended to be (mostly)
--   compatible with the matrix module we used in the first place.
--

module Data.Transformation where 

import Data.Angle

import Data.Glome.Vec hiding (translate, scale,x,y,z)
import qualified Data.Glome.Vec as G (translate, scale)

type Transformation = Xfm

-- Using the glomevec transformations

-- | Matrix multiplication
(!*!)::Transformation -> Transformation -> Transformation
(!*!) = xfm_mult

-- | Transform a point with a transformation matrix. Matrix-vector multiplication.
transformPoint::Transformation -> Vec -> Vec
transformPoint = xfm_point

-- | Transform a vector with a transformation matrix. Matrix-vector multiplication.
transformVector::Transformation -> Vec -> Vec
transformVector = xfm_vec

-- | Transform a vector with an inverse transformation matrix. Matrix-vector multiplication.
transformVector'::Transformation -> Vec -> Vec
transformVector' = invxfm_vec

-- | Identity matrix transformation
identityTransformation::Transformation
identityTransformation = ident_xfm

-- * Standard transformations
translate::Flt -> Flt -> Flt -> Transformation
translate x y z = G.translate (vec x y z)

scale::Flt -> Flt -> Flt -> Transformation
scale x y z = G.scale (vec x y z)

rotateX::Radians -> Transformation
rotateX = rotate (vec 1 0 0) .radians

rotateY::Radians -> Transformation
rotateY = rotate (vec 0 1 0) .radians

rotateZ::Radians -> Transformation
rotateZ = rotate (vec 0 0 1) .radians
