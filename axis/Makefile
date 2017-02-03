comp = gfortran
objects = resonant_axis.o main.o

main: main.o resonant_axis.o
	$(comp) -O2 $^ -o axis

main.o: main.f95 resonant_axis.mod
	$(comp) -O2 -c main.f95

resonant_axis.o resonant_axis.mod: resonant_axis.f95
	$(comp) -O2 -c $^

result: axis
	./axis

test:
	funit resonant_axis

clean:
	rm -f *.o *.mod axis
	funit --clean