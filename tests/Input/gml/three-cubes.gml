% test-cube.gml
%
% This file tests directional lights, spheres, union, and translate.

{ /color
  { /v /u /face   % bind arguments
    color         % surface color
    0.8 0.2 10.0  % kd ks n
  } cube
} /mkCube

1.0 0.0 0.0 point mkCube apply /redCube
0.0 1.0 0.0 point mkCube apply /greenCube
0.0 0.0 1.0 point mkCube apply /blueCube

redCube     -0.5 0.5  8.5 translate
greenCube    0.0 0.0  9.0 translate
blueCube     0.5 -0.5 9.5 translate
union union /scene

        % directional light
1.0 -1.0 1.0 point          % direction
1.0 0.0 0.0 point          % direction
1.0 1.0 1.0 point light /lcube % directional light

        % render
0.25 0.25 0.25 point      % ambient light
[ lcube ]                     % lights
scene                     % scene to render
8                         % tracing depth
60.0                      % field of view
480 480                   % image wid and height
"target/test-cubes.ppm"   % output file
render
