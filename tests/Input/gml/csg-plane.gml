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
  } cylinder
} /mkSphere

{ /color
  { /v /u /face			  % bind arguments
    color			  % surface color
    0.8 0.2 10.0		  % kd ks n
  } plane
} /mkPlane

1.0 0.0 0.0 point mkPlane apply
%0.0 -1.0 0.0 translate
%0.0 1.0 0.0 point mkSphere apply

%1.0 0.0 0.0 point mkPlane apply
%180.0 rotatex
%0.0 1.0 0.0 translate
%intersect

%1.0 0.0 0.0 point mkPlane apply
%90.0 rotatex
%0.0 0.0 -1.0 translate

%1.0 0.0 0.0 point mkPlane apply
%-90.0 rotatex
%0.0 0.0 1.0 translate
%intersect
%intersect
%
0.0 -1.0 0.0 translate
/scene
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
"csgplane.ppm"		  % output file
render
