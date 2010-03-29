% intercubes.gml
%
% Tests cube, translate, scale, rotation, and union.
%
% Last modified: 2003-11-01

{ /v /u /face
  0.7 0.3 0.3 point
  0.6 0.4 2.0
} cube -0.5 -0.5 -0.5 translate 0.1 1.0 1.0 scale

{ /v /u /face
  0.3 0.7 0.3 point
  0.6 0.4 2.0
} cube -0.5 -0.5 -0.5 translate 1.0 0.1 1.0 scale

union

{ /v /u /face
  0.3 0.3 0.7 point
  0.9 0.5 2.0
} cube -0.5 -0.5 -0.5 translate 1.0 1.0 0.1 scale

union

45.0 rotatey

45.0 rotatex

0.0 0.0 1.0 translate

 /scene

                                % directional light
0.8 -1.0 0.4 point                % direction
0.6  0.5 0.4 point light /l1      % directional light

0.0 1.5 -0.4 point  % origin
0.4 0.5 0.6 point pointlight /l2

0.5 0.5 0.5 point                 % ambient light
[ l1 l2 ]                         % lights
%1.0 0.01 0.001 point		  % light attenuation
scene                             % scene to render
20                                % tracing depth
90.0                              % field of view
640 480                           % image wid and height
"intercubes.ppm"                       % output file
render

