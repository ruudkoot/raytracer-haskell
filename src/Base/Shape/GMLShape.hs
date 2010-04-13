{-# LANGUAGE MultiParamTypeClasses #-}

module Base.Shape.GMLShape where

import Data.Vector

import Base.Shape
import Base.Shader

import GML.AST
import GML.Evaluate

data GMLShape = GMLShape
    { closureGetNormal' :: Closure
    , closureIntervals' :: Closure
    , closureUV         :: Closure
    , closureBoundingBox:: Closure }

instance Shape GMLShape Int where
    getNormal' = getNormalClosure'.closureGetNormal'
    intervals' = intervalsClosure'.closureIntervals'
    uv'        = uvClosure.closureUV
    boundingBox = boundingBoxClosure.closureBoundingBox
    
getNormalClosure' :: Closure -> Pt3D -> Vec3D
getNormalClosure' (e, c) p = let s = [Point p]
                                 (_, s', _) = evaluate (e, s, c)
                              in case s' of
                                [Point r] -> r
                                _ -> error ("error in normal closure: expected point on stack, found " ++ show s')
                            
                            
intervalsClosure' :: Closure -> Ray -> [Double]
intervalsClosure' (e, c) r = let s = [Point o,Point d]
                                 (_, s', _) = evaluate (e, s, c)
                                 o = rOrigin r
                                 d = rDirection r
                                 extractDoubles ev = case ev of
                                                      (BaseValue (Real rv)) -> rv
                                                      _ -> error ("error in intervals closure: expected array of reals on stack, found " ++ show s')
                             in case s' of
                                  [Array a] -> map extractDoubles a
                                  _ -> error ("error in intervals closure: expected array of reals on stack, found " ++ show s')
                             
                             
uvClosure :: Closure -> Pt3D -> (Int, Double, Double)
uvClosure (e, c) p = let s = [Point p]
                         (_, s', _) = evaluate (e, s, c)
                     in case s' of
                          [BaseValue (Int i), BaseValue (Real u), BaseValue (Real v)] -> (i, u, v)
                          _ -> error ("error in uv closure: expected Int, Real, Real on stack, found " ++ show s')
                          
boundingBoxClosure :: Closure -> Bbox
boundingBoxClosure (e, c) = let s = []
                                (_, s', _) = evaluate (e, s, c)
                            in case s' of
                                 [Point a, Point b] -> Bbox a b
                                 _ -> error ("error in uv closure: expected Point, Point on stack, found " ++ show s')
