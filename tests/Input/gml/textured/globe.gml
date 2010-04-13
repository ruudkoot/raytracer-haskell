%texture "globe" "globe.jpg"

%
%globe
%
{ /v /u /face
  "globe"
  1.0 0.5 5.0
} sphere
  180.0 rotatez
  /globe

%
%frame
%
{ /v /u /face
  face 0 eqi
  {0.5 0.5 1.0}
  {0.0 0.0 0.8}
  if point
  1.0 0.0 1.0
} cylinder
  0.0 -0.5 0.0 translate
  90.0 rotatex
  1.1 uscale
  /framecyl

{ /v /u /face
  0.5 0.5 0.5 point
  1.0 0.0 1.0
} cube
  -1.0 -0.5 -0.5 translate
  3.0 uscale
  /halfcube

{ /v /u /face
  0.8 0.8 0.8 point
  1.0 0.0 1.0
} cylinder
  0.0 -0.5 0.0 translate
  0.05 2.2 0.05 scale
  /connect

framecyl 1.1 1.1 0.2 scale
halfcube difference
framecyl difference
connect  union
/frame

%
%total scene
%
frame
globe
union
-30.0 rotatez
40.0 rotatey
/globeframe

{ /v /u /face
  0.8 0.2 0.8 point
  0.5 1.0 1.0
} cylinder
  0.7 0.2 0.7 scale
  /basebase

{ /v /u /face
  0.8 0.2 0.8 point
  0.3 8.0 1.0
} cylinder
  0.1 0.8 0.1 scale
  /basestand

basebase
basestand
union
/base

{
globeframe
0.0 2.0 0.0 translate
base
union
0.0 0.0 4.0 translate
/scene

-2.0 0.0 -1.0 point
1.0 1.0 1.0 point pointlight /l

0.3 0.3 0.3 point
[l]
scene 
1
90.0
320 200
"target/globe.ppm"
render
