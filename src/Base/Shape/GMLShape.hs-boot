{-# LANGUAGE MultiParamTypeClasses #-}
module Base.Shape.GMLShape where

import Input.GML.AST
import Base.Shape

data GMLShape = GMLShape () Closure Closure Closure Closure

instance Shape GMLShape ()