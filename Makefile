DIRS = axis

res:
	for d in $(DIRS); do (cd $$d; $(MAKE) ); done

prettify:
	find . -name "*.f90"|xargs fprettify -i 4

test:
	for d in $(DIRS); do (cd $$d; $(MAKE) test ); done

clean:
	for d in $(DIRS); do (cd $$d; $(MAKE) clean ); done
