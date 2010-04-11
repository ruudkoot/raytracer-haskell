% cube.gml
%
% OUTPUTS: cube0.ppm cube1.ppm cube2.ppm cube3.ppm cube4.ppm cube5.ppm
%
% test cube geometry and basic texturing
%

[ red green blue magenta yellow cyan ] /faces

{ /v /u /face
  0.5 0.5 0.5 point
  0.0 0.5 4.0
} cube /box

0.0 1.0 0.0 point /dir
1.0 1.0 0.0 point /col 
dir col light /l

{ /file /box
  0.0 0.0 0.0 point
  [l]
  box 0.0 0.0 3.0 translate
  1
  90.0
  320 200
  file
  render
} /doit


box 0.0 rotatex "target/cone0.ppm" doit apply
box 10.0 rotatex "target/cone10.ppm" doit apply
box 20.0 rotatex "target/cone20.ppm" doit apply
box 30.0 rotatex "target/cone30.ppm" doit apply
box 40.0 rotatex "target/cone40.ppm" doit apply
box 50.0 rotatex "target/cone50.ppm" doit apply
box 60.0 rotatex "target/cone60.ppm" doit apply
box 70.0 rotatex "target/cone70.ppm" doit apply
box 80.0 rotatex "target/cone80.ppm" doit apply
box 90.0 rotatex "target/cone90.ppm" doit apply
box 100.0 rotatex "target/cone100.ppm" doit apply
box 110.0 rotatex "target/cone110.ppm" doit apply
box 120.0 rotatex "target/cone120.ppm" doit apply
box 130.0 rotatex "target/cone130.ppm" doit apply
box 140.0 rotatex "target/cone140.ppm" doit apply
box 150.0 rotatex "target/cone150.ppm" doit apply
box 160.0 rotatex "target/cone160.ppm" doit apply
box 170.0 rotatex "target/cone170.ppm" doit apply
box 180.0 rotatex "target/cone180.ppm" doit apply
box 190.0 rotatex "target/cone190.ppm" doit apply
box 200.0 rotatex "target/cone200.ppm" doit apply
box 210.0 rotatex "target/cone210.ppm" doit apply
box 220.0 rotatex "target/cone220.ppm" doit apply
box 230.0 rotatex "target/cone230.ppm" doit apply
box 240.0 rotatex "target/cone240.ppm" doit apply
box 250.0 rotatex "target/cone250.ppm" doit apply
box 260.0 rotatex "target/cone260.ppm" doit apply
box 270.0 rotatex "target/cone270.ppm" doit apply
box 280.0 rotatex "target/cone280.ppm" doit apply
box 290.0 rotatex "target/cone290.ppm" doit apply
box 300.0 rotatex "target/cone300.ppm" doit apply
box 310.0 rotatex "target/cone310.ppm" doit apply
box 320.0 rotatex "target/cone320.ppm" doit apply
box 330.0 rotatex "target/cone330.ppm" doit apply
box 340.0 rotatex "target/cone340.ppm" doit apply
box 350.0 rotatex "target/cone350.ppm" doit apply
box 360.0 rotatex "target/cone360.ppm" doit apply
