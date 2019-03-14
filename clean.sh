#!/bin/bash
rm -f *.o *.mod comp.x info.out; funit --clean;
cd axis ; make clean ;
cd ../mercury ; make clean;
cd ../astdys_adapter ; rm -f *.o *.mod; funit --clean;
cd ../mercury_adapter ; rm -f *.o *.mod;
cd ../librations ; rm -f *.o *.mod; funit --clean;
