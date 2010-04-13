% test-cone.gml
%
% This file tests directional lights, cones, union, scale, and translate.
%
% Last modified: 2003-10-20

{ /color
  { /v /u /face			  % bind arguments
    color			  % surface color
    0.8 0.0 10.0		  % kd ks n
  } cone 1.0 2.0 1.0 scale
} /mkCone

1.0 0.0 0.0 point mkCone apply /redCone
0.0 1.0 0.0 point mkCone apply /greenCone
0.0 0.0 1.0 point mkCone apply /blueCone

redCone   -2.5 -1.0 3.0 translate
greenCone  0.0 -1.0 3.0 translate
blueCone   2.5 -1.0 3.0 translate
union union /scene

				% directional light
1.0 1.0 0.5 point		  % direction
0.75 0.75 0.75 point light /l	  % directional light

				% render
0.0 0.0 0.0 point		  % ambient light
[ l ]				  % lights
%1.0 0.0 0.0 point		  % light attenuation
scene				  % scene to render
2				  % tracing depth
90.0				  % field of view
480 480				  % image wid and height
"test-cone.ppm"		  % output file
render
