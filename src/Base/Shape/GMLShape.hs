{-# LANGUAGE MultiParamTypeClasses #-}

module Base.Shape.GMLShape where

import Postlude
import Base.Shape
import Base.Shader
import Data.Vector
import Input.GML.AST
import Input.GML.Evaluate

data GMLShape = GMLShape () Closure Closure Closure Closure

instance Shape GMLShape () where
    getNormal' (GMLShape () c1 _ _ _) v = getNormalCosure' c1 v
    intervals' (GMLShape () _ _ c3 _) r = intervalsClosure' c3 r
    uv         (GMLShape () _ _ _ c4) v = uvClosure c4 v
    
getNormalCosure' :: Closure -> Pt3D -> Vec3D
getNormalCosure' (e, c) p = let s = [Point p]
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
