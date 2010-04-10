%texture "globe" "globe.jpg"

{ /v /u /face
  "globe"
% u v 0.0 point
  1.0 0.0 1.0
} sphere
  -0.5 -0.5 -0.5 translate /globe

{ /file /box
  1.0 1.0 1.0 point
  []
  box 0.0 0.0 3.0 translate
  1
  90.0
  320 200
  file
  render
} /doit

% render front view
globe "globe0.ppm" doit apply

% render bottom view
globe 90.0 rotatex "target/globe1.ppm" doit apply

% render top view
globe -90.0 rotatex "target/globe2.ppm" doit apply

% render right view
globe 90.0 rotatey "target/globe3.ppm" doit apply

% render left view
globe -90.0 rotatey "target/globe4.ppm" doit apply

% render back view
globe 180.0 rotatex "target/globe5.ppm" doit apply
