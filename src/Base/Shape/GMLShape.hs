{-# LANGUAGE MultiParamTypeClasses #-}

module Base.Shape.GMLShape where

import Postlude
import Base.Shape
import Base.Shader
import Data.Vector
import Input.GML.AST
import Input.GML.Evaluate

data GMLShape = GMLShape
    { closureGetNormal' :: Closure
    , closureIntervals' :: Closure
    , closureUV         :: Closure }

instance Shape GMLShape () where
    getNormal' s v = getNormalClosure' (closureGetNormal' s) v
    intervals' s r = intervalsClosure' (closureIntervals' s) r
    uv         s v = uvClosure         (closureUV         s) v
    
getNormalClosure' :: Closure -> Pt3D -> Vec3D
getNormalClosure' (e, c) p = let s = [Point p]
                                 (e', s', c') = evaluate (e, s, c)
                              in case s' of
                                [Point r] -> r
                                _ -> error ("error in normal closure: expected point on stack, found " ++ show s')
                            
                            
intervalsClosure' :: Closure -> Ray -> [Double]
intervalsClosure' (e, c) r = let s = [Point o,Point d]
                                 (e', s', c') = evaluate (e, s, c)
                                 o = rOrigin r
                                 d = rDirection r
                                 extractDoubles e = case e of
                                                      (BaseValue (Real r)) -> r
                                                      _ -> error ("error in intervals closure: expected array of reals on stack, found " ++ show s')
                             in case s' of
                                  [Array a] -> map extractDoubles a
                                  _ -> error ("error in intervals closure: expected array of reals on stack, found " ++ show s')
                             
                             
uvClosure :: Closure -> Pt3D -> SurfaceCoord
uvClosure (e, c) p = let s = [Point p]
                         (e', s', c') = evaluate (e, s, c)
                     in case s' of
                          [BaseValue (Int i), BaseValue (Real u), BaseValue (Real v)] -> (i, u, v)
                          _ -> error ("error in uv closure: expected Int, Real, Real on stack, found " ++ show s')
