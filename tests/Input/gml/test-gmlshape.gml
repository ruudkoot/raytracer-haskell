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

{ /v /u /face
  v floor /i
  u floor /j
  i j addi 2 modi 0 eqi
  { red 1.0 0.0 1.0 }
  { black 0.0 1.0 20.0 }
  if
} fakeplane apply 0.0 -2.0 0.0 translate /p1

{ /v /u /face
  v floor /i
  u floor /j
  i j addi 2 modi 0 eqi
  { white 1.0 0.0 1.0 }
  { green 1.0 0.0 1.0 }
  if
} fakeplane apply 45.0 rotatey -90.0 rotatex 0.0 0.0 8.0 translate /p2

p1 p2 union /scene
				% render
1.0 1.0 1.0 point		  % ambient light
[ ]				  % lights
%1.0 0.0 0.0 point		  % light attenuation
scene				  % scene to render
2				  % tracing depth
90.0				  % field of view
480 480				  % image wid and height
"test-plane.ppm"		  % output file
render
