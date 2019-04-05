DIRS = axis

res:
	./init.sh
#	for d in $(DIRS); do (cd $$d; $(MAKE) ); done

source:
	curl "http://hamilton.dm.unipi.it/~astdys2/catalogs/allnum.cat" -o input/allnum.cat
	@echo AstDyS Catalog was downloaded

prettify:
	find . -name "*.f90"|xargs fprettify -i 4

test:
	funit global_parameters
	cp global_parameters.f90 ./astdys_adapter/
	cp global_parameters.f90 ./axis/
	cp global_parameters.f90 ./librations/
	cd ./astdys_adapter ; funit astdys_adapter; rm -f global_parameters.f90
	cd ./axis ; make test ; rm -f global_parameters.f90
	cd ./librations ; funit librations_support; rm -f global_parameters.f90
	./clean.sh

clean:
	./clean.sh
	funit --clean
#	for d in $(DIRS); do (cd $$d; $(MAKE) clean ); done
