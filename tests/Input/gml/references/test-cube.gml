% test-cube.gml
%
% This file tests cubes, union, translate, and rotation.
%
% Last modified: 2003-10-17

1.0  0.0  0.0  point /red
0.0  1.0  0.0  point /green
0.0  0.0  1.0  point /blue
1.0  0.0  1.0  point /magenta
1.0  1.0  0.0  point /yellow
0.0  1.0  1.0  point /cyan

[ red		% front  (z=0)
  green		% back   (z=1)
  blue		% left   (x=0)
  magenta	% right  (x=1)
  yellow	% top    (y=1)
  cyan		% bottom (y=0)
] /colors

% a cube centered at the origin.
{ /v /u /face		  % bind arguments
  colors face get	  % surface color
  1.0 0.0 0.0		  % kd ks n
} cube -0.5 -0.5 -0.5 translate /box

box 45.0 rotatex   -2.5  0.0 3.0 translate
box 45.0 rotatey    0.0  0.0 3.0 translate
box 45.0 rotatez    2.5  0.0 3.0 translate
box 180.0 rotatex   0.0  2.5 3.0 translate
box                 0.0 -2.5 3.0 translate
union union union union /scene

				% render
1.0 1.0 1.0 point		  % ambient light
[ ]				  % lights
1.0 0.0 0.0 point		  % light attenuation
scene				  % scene to render
2				  % tracing depth
90.0				  % field of view
480 480				  % image wid and height
"test-cube.ppm"			  % output file
render
