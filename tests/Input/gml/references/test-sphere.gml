% test-sphere.gml
%
% This file tests directional lights, spheres, union, and translate.
%
% Last modified: 2003-10-17

{ /color
  { /v /u /face			  % bind arguments
    color			  % surface color
    0.8 0.2 10.0		  % kd ks n
  } sphere
} /mkSphere

1.0 0.0 0.0 point mkSphere apply /redSphere
0.0 1.0 0.0 point mkSphere apply /greenSphere
0.0 0.0 1.0 point mkSphere apply /blueSphere

redSphere   -2.5 0.0 3.0 translate
greenSphere  0.0 0.0 3.0 translate
blueSphere   2.5 0.0 3.0 translate
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
"test-sphere.ppm"		  % output file
render
