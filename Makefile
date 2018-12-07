FC = gfortran
FLAGS = -ffree-form

all: autodiff.o example

autodiff.o: autodiff.f
	$(FC) $(FLAGS) -c $< -o $@

example: example.f autodiff.o
	$(FC) $(FLAGS) $? -o $@

clean:
	rm example *.mod *.o
