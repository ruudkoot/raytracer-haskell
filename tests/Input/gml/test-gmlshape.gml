% test-sphere.gml
%
% This file tests directional lights, spheres, union, and translate.

0.0  0.0  0.0  point /black
1.0  1.0  1.0  point /white
1.0  0.0  0.0  point /red
0.0  1.0  0.0  point /green
0.0  0.0  1.0  point /blue
1.0  0.0  1.0  point /magenta
1.0  1.0  0.0  point /yellow
0.0  1.0  1.0  point /cyan


{ /v1 /v2
  v1 getx v2 getx mulf
  v1 gety v2 gety mulf addf
  v1 getz v2 getz mulf addf
} /dot

{ /p
p p dot apply sqrt
} /magnitude

{ /v
  1.0 v magnitude apply divf /s	% s = sqrt(1.0/v dot v)
  s v getx mulf				% push s*x
  s v gety mulf				% push s*y
  s v getz mulf				% push s*z
  point					% make normalized vector
} /normalize

{ /c /b /a
b b mulf 4.0 a c mulf mulf subf /discr
discr 0.0 lessf
{
[ ]
}
{
discr sqrt /sqd
b negf sqd addf a 2.0 mulf divf /x1
b negf sqd subf a 2.0 mulf divf /x2
[ x1 x2 ]
} if
}
/solveQuadratic

{ /shader
{ /p
0.0 1.0 0.0 point
}

{ /p
p gety 0.0 lessf
}

{ /o /r
o gety /oy
r gety /ry
oy 0.0 eqf
{ [ ] }
{ 0.0 oy ry mulf lessf
{ [] }
{ oy negf ry divf /ret
[ ret ]
} if
} if
}

{ /p
p getz
p getx
0
} shader gmlshape } /fakeplane


{ /shader
{ /p
p normalize apply
}

{ /p
p magnitude apply 1.0 lessf
}

{ /k /dir
dir dir dot apply /a
k dir dot apply 2.0 mulf /b
k k dot apply 1.0 subf /c
a b c solveQuadratic apply
}

{ /p
p gety acos /theta
p getz p getx atan2 /phi
3.141592653589793238 /pi
pi theta subf pi divf
phi pi 2.0 mulf divf
0
} shader gmlshape } /fakesphere

{ /color
  { /v /u /face   % bind arguments
    color         % surface color
    0.8 0.2 10.0  % kd ks n
  } fakesphere apply
} /mkSphere

1.0 0.0 0.0 point mkSphere apply /redSphere
0.0 1.0 0.0 point mkSphere apply /greenSphere
0.0 0.0 1.0 point mkSphere apply /blueSphere

redSphere     -2.5 2.0  9.0 translate
greenSphere    0.0 0.0  9.0 translate
blueSphere     2.5 0.0 9.0 translate
union union /scene

        % directional light
1.0 -1.0 1.0 point          % direction
1.0 0.0 0.0 point          % direction
1.0 1.0 1.0 point light /l % directional light

        % render
0.25 0.25 0.25 point      % ambient light
[ l ]                     % lights
scene                     % scene to render
8                         % tracing depth
60.0                      % field of view
480 480                   % image wid and height
"target/test-sphere.ppm"  % output file
render

