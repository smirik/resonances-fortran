#!/bin/bash
cd mercury ; ./compile.sh;
cd ../axis ; make;
cp *.mod ../; cp *.mod ../librations/
cp global_parameters.mod ../astdys_adapter/;
cp global_parameters.mod ../mercury_adapter/;
cp global_parameters.mod ../librations/;

cd ../astdys_adapter ;
gfortran -O2 -c astdys_adapter.f90;
cp astdys_adapter.mod ../ ;
cd ../mercury_adapter ;
gfortran -O2 -c mercury_adapter.f90;
cp mercury_adapter.mod ../ ;
cd ../librations ;
gfortran -O2 -c simp_res_str.f90;
gfortran -O2 -c librations_support.f90;
gfortran -O2 -c librations.f90;
cp librations.mod ../;
cd ../
gfortran -O2 axis/*.o astdys_adapter/*.o mercury_adapter/*.o librations/*.o compositor.f90 -o comp.x;
mkdir -p aeibase
mkdir -p aei_planet
mkdir -p id_matrices
mkdir -p wd
