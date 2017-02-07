comp= gfortran
firstmodule= global_parameters

module_sources= id_matrix.f95 \
                resonant_axis.f95 \
                resonance_finder.f95

module_objects = $(patsubst %.f95,%.o,$(module_sources))
module_mods = $(patsubst %.f95,%.mod,$(module_sources))

main:  $(firstmodule).o $(module_objects) main.o
	$(comp) -O2 $^ -o axis

main.o: main.f95
	$(comp) -O2 -c main.f95

$(module_objects) $(module_mods): $(module_sources)# $(firstmodule).mod
	for i in $^;do $(comp) -O2 -c $$i;echo "gfortran -O2 -c" $$i;done

$(firstmodule).o $(firstmodule).mod: $(firstmodule).f95
	$(comp) -O2 -c $^

test:
	funit resonant_axis

clean:
	rm -f *.o *.mod axis id_matrix_*
#	funit --clean
