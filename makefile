surface_gen: surface_gen.o
	gfortran surface_gen.f90 reflect_nozzle.f90 hermite_nozzle.f90 -o surface_gen.o
