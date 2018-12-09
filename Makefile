FC = gfortran
FLAGS =

all: autodiff.o example

autodiff.o: autodiff.f90
	$(FC) $(FLAGS) -c $< -o $@

example: example.f90 autodiff.o
	$(FC) $(FLAGS) $^ -o $@

clean:
	rm example *.mod *.o
