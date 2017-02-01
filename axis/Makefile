main: main.o functions.o resonance_table.o
	gfortran -o axis main.o functions.o resonance_table.o

functions.o: functions.f90  
	gfortran -c functions.f90

resonance_table.o: resonance_table.f90
	gfortran -c resonance_table.f90

main.o: main.f90
	gfortran -c main.f90

clean:
	rm -f *.o axis
