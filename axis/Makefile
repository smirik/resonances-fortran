comp= gfortran
firstmodule= global_parameters

module_sources= ../global_parameters.f90 \
        resonant_axis.f90 \
        idmatrix_2body_mod.f90 \
        idmatrix_3body_mod.f90 \
        idmatrix.f90 \
        resonance_finder.f90

module_objects = $(patsubst %.f90,%.o,$(module_sources))
module_mods = $(patsubst %.f90,%.mod,$(module_sources))

$(module_objects) $(module_mods): $(module_sources)
	for i in $^;do $(comp) -O2 -c $$i;echo "gfortran -O2 -c" $$i;done

#Run tests after all object files created
test:
	echo 'tests in axis'
	funit resonant_axis
	funit idmatrix

clean:
	rm -f *.o *.mod axis
	funit --clean
