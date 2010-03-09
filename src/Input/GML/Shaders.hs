module Input.GML.Shaders where

import Shared.Colour
import Base.Shader

import Input.GML.AST

gmlShader::Closure -> Shader
gmlShader (env,c) = Shader sf
        where sf (u,v,face) = let 
                              in ShaderResult (Colour (1.0,1.0,1.0)) 1.0 0.0 1.0
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
