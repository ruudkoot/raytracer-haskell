{-# LANGUAGE MultiParamTypeClasses #-}
module Base.Shape.GMLShape where

import Base.Shape
import GML.AST

data GMLShape = GMLShape
    { closureGetNormal' :: Closure
    , closureIntervals' :: Closure
    , closureUV         :: Closure }

instance Shape GMLShape Int
