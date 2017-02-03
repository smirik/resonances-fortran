comp = gfortran
objects = resonant_axis.o main.o

main: main.o resonant_axis.o global_parameters.o
	$(comp) -O2 $^ -o axis

main.o: main.f95 resonant_axis.mod global_parameters.mod
	$(comp) -O2 -c main.f95

resonant_axis.o resonant_axis.mod: resonant_axis.f95
	$(comp) -O2 -c $^

global_parameters.o global_parameters.mod: global_parameters.f95
	$(comp) -O2 -c $^

test:
	funit resonant_axis

clean:
	rm -f *.o *.mod axis
	funit --clean