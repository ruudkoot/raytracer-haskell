{-# LANGUAGE MultiParamTypeClasses #-}
module Base.Shape.GMLShape where

import Input.GML.AST
import Base.Shape

data GMLShape = GMLShape
    { closureGetNormal' :: Closure
    , closureInside     :: Closure
    , closureIntervals' :: Closure
    , closureUV         :: Closure }

instance Shape GMLShape ()
