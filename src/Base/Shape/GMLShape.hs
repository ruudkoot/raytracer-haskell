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
    inside     (GMLShape () _ c2 _ _)  p = insideClosure c2 p
    intervals' (GMLShape () _ _ c3 _) r = intervalsClosure' c3 r
    uv         (GMLShape () _ _ _ c4) v = intervalsUv c4 v
    
getNormalCosure' :: Closure -> Pt3D -> Vec3D
getNormalCosure' (e, c) p = let s = [Point p]
                                (e', s', c') = evaluate (e, s, c)
                                [Point r] = s'
                            in r
                            
insideClosure :: Closure -> Pt3D -> Bool
insideClosure (e, c) p = let s = [Point p]
                             (e', s', c') = evaluate (e, s, c)
                             [BaseValue (Boolean b)] = s'
                             x = getX3D p
                             y = getY3D p
                             z = getZ3D p
                          in b
                            
intervalsClosure' :: Closure -> Ray -> [Double]
intervalsClosure' (e, c) r = let s = [Point o,Point d]
                                 (e', s', c') = evaluate (e, s, c)
                                 [Array a] = s'
                                 o = rOrigin r
                                 d = rDirection r
                             in map extractDoubles a
                             where extractDoubles (BaseValue (Real r)) = r
                             
intervalsUv :: Closure -> Pt3D -> SurfaceCoord
intervalsUv (e, c) p = let s = [Point p]
                           (e', s', c') = evaluate (e, s, c)
                           [BaseValue (Int i), BaseValue (Real u), BaseValue (Real v)] = s'
                        in (i, u, v)