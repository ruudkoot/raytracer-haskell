%texture "globe" "globe.jpg"

{ /v /u /face
  "globe"
  1.0 0.0 1.0
} cube
  -0.5 -0.5 -0.5 translate /box

{ /file /box
  1.0 1.0 1.0 point
  []
  box 0.0 0.0 2.0 translate
  1
  90.0
  320 200
  file
  render
} /doit

% render front view
box "cube0.ppm" doit apply

% render bottom view
box 90.0 rotatex "target/cube1.ppm" doit apply

% render top view
box -90.0 rotatex "target/cube2.ppm" doit apply

% render right view
box 90.0 rotatey "target/cube3.ppm" doit apply

% render left view
box -90.0 rotatey "target/cube4.ppm" doit apply

% render back view
box 180.0 rotatex "target/cube5.ppm" doit apply

