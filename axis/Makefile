comp= gfortran
firstmodule= global_parameters

module_sources= resonant_axis.f90 \
                id_matrix.f90 \
                resonance_finder.f90

module_objects = $(patsubst %.f90,%.o,$(module_sources))
module_mods = $(patsubst %.f90,%.mod,$(module_sources))

main:  $(firstmodule).o $(module_objects) main.o
	$(comp) -O2 $^ -o axis

main.o: main.f90
	$(comp) -O2 -c main.f90

$(module_objects) $(module_mods): $(module_sources)# $(firstmodule).mod
	for i in $^;do $(comp) -O2 -c $$i;echo "gfortran -O2 -c" $$i;done

$(firstmodule).o $(firstmodule).mod: $(firstmodule).f90
	$(comp) -O2 -c $^

test:
	funit resonant_axis

clean:
	rm -f *.o *.mod axis id_matrix_*
	funit --clean
