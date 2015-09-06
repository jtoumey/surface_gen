#
gfortran surface_gen.f90 reflect_nozzle.f90 write_points_2d.f90 write_points_3d.f90 query_file.f90 hermite_nozzle.f90 -o surface_gen.o
#
./surface_gen.o <<< "

1

test_3.dat

x,-1

12

"

diff cd_nozzle_2d.dat ./test/cd_nozzle_2d_bl.dat | tee ./test/diff_2d.dat
diff cd_nozzle_3d.dat ./test/cd_nozzle_3d_bl.dat | tee ./test/diff_3d.dat
