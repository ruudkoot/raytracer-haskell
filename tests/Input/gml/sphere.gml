{ /v /u /face
  1.0 0.0 0.0 point
  1.0
  0.0
  1.0
} sphere
0.2 uscale
0.0 0.0 10.0 translate
/scene

0.8 0.8 0.8 point % ambient
[ ]               % lights
scene             % obj
0                 % depth
60.0              % FOV 
200               % width
200               % height
"target/foo.ppm"  % target
render