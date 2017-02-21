#!/bin/bash
rm *.o *.mod comp.x
cd axis ; make clean ;
cd ../mercury ; rm -f *.dmp *.clo *.out *.tmp *.aei *.in close6 element6 mercury6;
cd ../integrator ; rm *.o *.mod;
cd ../librations ; rm *.o *.mod;
cd ../; rm -rf wd