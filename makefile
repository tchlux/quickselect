COMPILER=gfortran

test: test.o quickselect.o
	$(COMPILER) -o test test.o quickselect.o

test.o: test.f90 quickselect.o
	$(COMPILER) -c test.f90

quickselect.o: quickselect.f90
	$(COMPILER) -c quickselect.f90


prof: test.f90 quickselect.f90
	$(COMPILER) -pg -c quickselect.f90
	$(COMPILER) -pg -c test.f90
	$(COMPILER) -pg -static test.o quickselect.o -o prof_test

clean:
	rm -f quickselect.o quickselect.mod test.o test 
