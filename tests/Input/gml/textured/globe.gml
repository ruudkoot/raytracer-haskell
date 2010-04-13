%texture "globe" "globe.jpg"
%texture "wood" "wood.jpg"

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
%Globe frame
frame
globe
union
-30.0 rotatez
40.0 rotatey
/globeframe

%
% Globe base
%
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

%
%Full globe
%
globeframe
0.0 2.0 0.0 translate
base
union
/fullglobe

%
%table
%
{ /v /u /face
  "wood"
  0.5 1.0 1.0
} cube /tablewood

{
tablewood
0.4 5.0 0.4 scale } /leg

tablewood
8.0 0.4 10.0 scale
leg apply 0.0 -5.0 0.0 translate
union
leg apply 7.6 -5.0 0.0 translate
union
leg apply 0.0 -5.0 9.6 translate
union
leg apply 7.6 -5.0 9.6 translate
union
-4.0 -0.4 -5.0 translate
/table

%
%ground plane
%
{ /v /u /face
  0.8 0.3 0.5 point
  0.5 1.0 1.0
} plane /floorplane


%
%scene
%
table
0.6 uscale
floorplane
0.0 -3.0 0.0 translate
union
fullglobe
-1.0 0.0 -1.0 translate
union
-4.0 -3.0 10.0 translate
/scene

-10.0 4.0 -1.0 point
1.0 1.0 1.0 point pointlight /l

{ /angle /z /y /x /object
   object
   x -1.0 mulf
   y -1.0 mulf
   z -1.0 mulf
   translate
   angle rotatey
   x y z translate
} /rotateAround

{ /angle /z /y /x /file
0.3 0.3 0.3 point
[l]
scene x y z angle rotateAround apply
2
90.0
320 200
file
render
} /drawRotated

"target/globe0.ppm" -2.0 -1.5 8.0 0.0 drawRotated apply
"target/globe30.ppm" -2.0 -1.5 8.0 30.0 drawRotated apply
"target/globe60.ppm" -2.0 -1.5 8.0 60.0 drawRotated apply
"target/globe90.ppm" -2.0 -1.5 8.0 90.0 drawRotated apply
"target/globe120.ppm" -2.0 -1.5 8.0 120.0 drawRotated apply
"target/globe150.ppm" -2.0 -1.5 8.0 150.0 drawRotated apply
"target/globe180.ppm" -2.0 -1.5 8.0 180.0 drawRotated apply
"target/globe210.ppm" -2.0 -1.5 8.0 210.0 drawRotated apply
"target/globe240.ppm" -2.0 -1.5 8.0 240.0 drawRotated apply
"target/globe270.ppm" -2.0 -1.5 8.0 270.0 drawRotated apply
"target/globe300.ppm" -2.0 -1.5 8.0 300.0 drawRotated apply
"target/globe330.ppm" -2.0 -1.5 8.0 330.0 drawRotated apply

