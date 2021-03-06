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

1.0  1.0  1.0  point /white
0.3  0.3  0.3  point /grey

%
%ground plane
%
{ /v /u /face
  v floor /i
  u floor /j
  i j addi 2 modi 0 eqi
  { white 1.0 0.2 1.0 }
  { grey 1.0 0.2 1.0 }
  if
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
640 480
file
render
} /drawRotated

"target/globe000.ppm" -2.0 -1.5 8.0 0.0 drawRotated apply
"target/globe010.ppm" -2.0 -1.5 8.0 10.0 drawRotated apply
"target/globe020.ppm" -2.0 -1.5 8.0 20.0 drawRotated apply
"target/globe030.ppm" -2.0 -1.5 8.0 30.0 drawRotated apply
"target/globe040.ppm" -2.0 -1.5 8.0 40.0 drawRotated apply
"target/globe050.ppm" -2.0 -1.5 8.0 50.0 drawRotated apply
"target/globe060.ppm" -2.0 -1.5 8.0 60.0 drawRotated apply
"target/globe070.ppm" -2.0 -1.5 8.0 70.0 drawRotated apply
"target/globe080.ppm" -2.0 -1.5 8.0 80.0 drawRotated apply
"target/globe090.ppm" -2.0 -1.5 8.0 90.0 drawRotated apply
"target/globe100.ppm" -2.0 -1.5 8.0 100.0 drawRotated apply
"target/globe110.ppm" -2.0 -1.5 8.0 110.0 drawRotated apply
"target/globe120.ppm" -2.0 -1.5 8.0 120.0 drawRotated apply
"target/globe130.ppm" -2.0 -1.5 8.0 130.0 drawRotated apply
"target/globe140.ppm" -2.0 -1.5 8.0 140.0 drawRotated apply
"target/globe150.ppm" -2.0 -1.5 8.0 150.0 drawRotated apply
"target/globe160.ppm" -2.0 -1.5 8.0 160.0 drawRotated apply
"target/globe170.ppm" -2.0 -1.5 8.0 170.0 drawRotated apply
"target/globe180.ppm" -2.0 -1.5 8.0 180.0 drawRotated apply
"target/globe190.ppm" -2.0 -1.5 8.0 190.0 drawRotated apply
"target/globe200.ppm" -2.0 -1.5 8.0 200.0 drawRotated apply
"target/globe210.ppm" -2.0 -1.5 8.0 210.0 drawRotated apply
"target/globe220.ppm" -2.0 -1.5 8.0 220.0 drawRotated apply
"target/globe230.ppm" -2.0 -1.5 8.0 230.0 drawRotated apply
"target/globe240.ppm" -2.0 -1.5 8.0 240.0 drawRotated apply
"target/globe250.ppm" -2.0 -1.5 8.0 250.0 drawRotated apply
"target/globe260.ppm" -2.0 -1.5 8.0 260.0 drawRotated apply
"target/globe270.ppm" -2.0 -1.5 8.0 270.0 drawRotated apply
"target/globe280.ppm" -2.0 -1.5 8.0 280.0 drawRotated apply
"target/globe290.ppm" -2.0 -1.5 8.0 290.0 drawRotated apply
"target/globe300.ppm" -2.0 -1.5 8.0 300.0 drawRotated apply
"target/globe310.ppm" -2.0 -1.5 8.0 310.0 drawRotated apply
"target/globe320.ppm" -2.0 -1.5 8.0 320.0 drawRotated apply
"target/globe330.ppm" -2.0 -1.5 8.0 330.0 drawRotated apply
"target/globe340.ppm" -2.0 -1.5 8.0 340.0 drawRotated apply
"target/globe350.ppm" -2.0 -1.5 8.0 350.0 drawRotated apply
