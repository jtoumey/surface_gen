surface_gen.o: surface_gen.o reflect_nozzle.o
	gfortran surface_gen.o reflect_nozzle.o -o surface_gen.o
surface_gen.o: surface_gen.f90
	gfortran surface_gen.f90 -o surface_gen.o
reflect_nozzle.o: reflect_nozzle.f90
	gfortran reflect_nozzle.f90 -o reflect_nozzle.o
