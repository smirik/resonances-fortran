#!/bin/bash
rm -f *.o *.mod comp.x info.out
cd axis ; make clean ;
cd ../mercury ; rm -f *.dmp *.clo *.out *.tmp *.aei *.in close6 element6 mercury6;
cd ../integrator ; rm -f *.o *.mod;
cd ../librations ; rm -f *.o *.mod; funit --clean;
cd ../wd ; rm -f *.circ* *.per* *.rp? *.phout* *.smooth*
#cd ../; rm -rf wd