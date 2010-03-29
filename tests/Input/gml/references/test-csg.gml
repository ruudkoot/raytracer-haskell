% test-csg.gml
%
% This file tests directional lights, spheres, cylinders,
% transformations, and csg operations.
%
% Last modified: 2003-10-25

{ /color
  { /v /u /face			  % bind arguments
    color			  % surface color
    0.8 0.2 10.0		  % kd ks n
  } sphere
} /mkSphere

{ /color
  { /v /u /face			  % bind arguments
    color			  % surface color
    0.8 0.2 10.0		  % kd ks n
  } cylinder
} /mkCylinder

0.0 1.0 0.0 point mkSphere apply /s

1.0 0.0 0.0 point mkCylinder apply
  0.0 -0.5 0.0 translate
  0.5 2.2 0.5 scale 
  90.0 rotatex /c

s c union /u
s c intersect /i
s c difference /d

u   -2.5 0.0 3.0 translate
i -90.0 rotatex 0.0 0.0 3.0 translate
d   2.5 0.0 3.0 translate
union union /scene

				% directional light
1.0 -1.0 0.0 point		  % direction
1.0  1.0 1.0 point light /l	  % directional light

				% render
0.25 0.25 0.25 point		  % ambient light
[ l ]				  % lights
%1.0 0.0 0.0 point		  % light attenuation
scene				  % scene to render
2				  % tracing depth
90.0				  % field of view
480 480				  % image wid and height
"test-csg.ppm"		  % output file
render
