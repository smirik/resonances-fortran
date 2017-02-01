f90comp = gfortran
objects = resonant_axis.o main.o

main: main.o resonant_axis.o
	$(f90comp) -o axis $(objects)

resonant_axis.mod: resonant_axis.o resonant_axis.f90
	$(f90comp) -c resonant_axis.f90

resonant_axis.o: resonant_axis.f90
	$(f90comp) -c resonant_axis.f90

main.o: main.f90 resonant_axis.mod
	$(f90comp) -c main.f90

test:
	funit resonant_axis

clean:
	rm -f *.o *.mod axis
	funit --clean