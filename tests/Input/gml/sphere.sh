#!/bin/sh

echo { /v /u /face
echo   1.0 0.0 0.0 point
echo   1.0
echo   0.0
echo   1.0
echo } sphere
echo 0.1 uscale
echo 0.0 0.1 $1 translate
echo /scene
echo 
echo 0.8 0.8 0.8 point
echo [ ]
echo scene
echo 0
echo 60.0
echo 200
echo 200
echo \"target/foo$1.ppm\"
echo render