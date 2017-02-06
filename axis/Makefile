comp= gfortran
firstmodule= global_parameters

module_sources=resonant_axis.f95
module_objects = $(patsubst %.f95,%.o,$(module_sources))
module_mods = $(patsubst %.f95,%.mod,$(module_sources))

main: main.o $(module_objects) $(firstmodule).o
	$(comp) -O2 $^ -o axis

main.o: main.f95 $(module_mods) $(firstmodule).mod
	$(comp) -O2 -c main.f95

$(module_objects) $(module_mods): $(module_sources) $(firstmodule).mod
	for i in "$<";do $(comp) -O2 -c $$i;echo "gfortran -O2 -c" $$i;done

$(firstmodule).o $(firstmodule).mod: $(firstmodule).f95
	$(comp) -O2 -c $^

test:
	funit resonant_axis

clean:
	rm -f *.o *.mod axis
	funit --clean
