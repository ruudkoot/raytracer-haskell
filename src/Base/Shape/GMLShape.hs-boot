{-# LANGUAGE MultiParamTypeClasses #-}
module Base.Shape.GMLShape where

import Input.GML.AST
import Base.Shape

data GMLShape = GMLShape
    { closureGetNormal' :: Closure
    , closureIntervals' :: Closure
    , closureUV         :: Closure }

instance Shape GMLShape ()
