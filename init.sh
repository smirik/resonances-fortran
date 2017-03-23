#!/bin/bash
cd mercury ; ./compile.sh;
cd ../axis ; make;
cp *.mod ../;
cp global_parameters.mod ../integrator/;
cp global_parameters.mod ../librations/;

cd ../integrator ; gfortran -O2 -c integrator.f90;
cp integrator.mod ../;
cd ../librations ; gfortran -O2 -c librations.f90;
cp librations.mod ../
cd ../
gfortran -O2 axis/*.o integrator/*.o librations/*.o compositor.f90 -o comp.x;
mkdir -p wd
mkdir -p aeibase
mkdir -p id_matrices
#mkdir -p axis/id_matrices_2body