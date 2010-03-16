module Input.GML.Shaders where

import Data.Colour
import Base.Shader

import Input.GML.AST

gmlShader :: Closure -> Shader
gmlShader (env, c) = Shader sf
    where sf (face, u, v) = SurfaceProperty { colour                        = Colour (1.0, 1.0, 1.0)
                                            , diffuseReflectionCoefficient  = undefined
                                            , specularReflectionCoefficient = undefined
                                            , phongExponent                 = undefined
                                            }
{-
gmlShader::Closure -> Shader
gmlShader (env,c) = Shader sf
        where sf (u,v,face) = let (_,(BaseValue (Real n):
                                      BaseValue (Real ks):
                                      BaseValue (Real kd):
                                      BaseValue (Real b):
                                      BaseValue (Real g):
                                      BaseValue (Real r):_),_) = evaluate (env,BaseValue .Int face:BaseValue .Real v:BaseValue .Real u:[],c)
                              in ShaderResult (Colour (r,g,b)) kd ks n-}
