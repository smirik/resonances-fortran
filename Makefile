DIRS = axis

res:
	./init.sh
#	for d in $(DIRS); do (cd $$d; $(MAKE) ); done

prettify:
	find . -name "*.f95"|xargs fprettify -i 4

test:
	for d in $(DIRS); do (cd $$d; $(MAKE) test ); done

clean:
	./clean.sh
#	for d in $(DIRS); do (cd $$d; $(MAKE) clean ); done
