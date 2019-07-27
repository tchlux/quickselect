COMPILER=gfortran

test: fast_select.o test.o
	$(COMPILER) -o test test.o fast_select.o

test.o: test.f90 fast_select.o
	$(COMPILER) -c test.f90

fast_select.o: fast_select.f90
	$(COMPILER) -c fast_select.f90


prof: test.f90 fast_select.f90
	$(COMPILER) -pg -c fast_select.f90
	$(COMPILER) -pg -c test.f90
	$(COMPILER) -pg -static test.o fast_select.o -o prof_test
	gprof prof_test

clean:
	rm -f fast_select.o fast_select.mod test.o test 
