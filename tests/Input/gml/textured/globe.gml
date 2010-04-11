%texture "globe" "globe.jpg"

%
%globe
%
{ /v /u /face
  "globe"
  1.0 0.0 1.0
} sphere
  180.0 rotatez
  /globe

%
%frame
%
{ /v /u /face
  face 0 eqi
  {0.5 0.5 1.0}
  {0.5 0.5 0.8}
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
  0.0 -0.5 -0.5 translate
  3.0 uscale
  /halfcube

{ /v /u /face
  0.8 0.8 0.8 point
  1.0 0.0 1.0
} cylinder
  0.0 -0.5 0.0 translate
  0.1 1.1 0.1 scale
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
40.0 rotatey
0.0 -1.0 4.0 translate
/scene

1.0 1.0 1.0 point
[]
scene 
1
90.0
320 200
"target/globe.ppm"
render