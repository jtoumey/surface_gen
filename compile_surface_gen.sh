#
gfortran surface_gen.f90 reflect_nozzle.f90 write_points.f90 write_points_2d.f90 hermite_nozzle.f90 -o surface_gen.o
#
./surface_gen.o <<< "

1

test_3.dat

x,-1

12

"

diff output_points.dat output_points_bl.dat | tee output_test_2D.dat
diff CD_nozzle_3D.dat CD_nozzle_3D_bl.dat | tee output_test_3D.dat
