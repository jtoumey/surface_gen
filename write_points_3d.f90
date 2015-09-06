!*************************************************************************!
!                                                                         !
!  Subroutine:   WRITE_POINTS.f90                                         !
!                                                                         !
!  Programmer:   Julian M. Toumey                                         !
!                Madison, WI                                              !
!                                                                         !
!  Date:         August 2015                                              !
!                                                                         !
!  Language:     FORTRAN 90                                               !
!                                                                         !
!  Description:  This subroutine writes a 3D geometry for a converging-   !
!                diverging nozzle based on an input file with one curve   !
!                for the nozzle surface, a reflection axis, and a         !
!                granularity for the number of points in the radial       !
!                direction.                                               !
!                                                                         !
!  Inputs:       num_points      number of points in the x-dir for curve  !
!                x               array of x coordinates for curve         !
!                z               array of corresponding z coordinates     !
!                reflect_value   value of reflection axis for curve       !
!                                                                         !
!  Output Files: cd_nozzle_3D.dat   Output file containing x, y, and z    !
!                                   coordinates of points on the nozzle   !
!                                   surface.                              !
!                                                                         !
!*************************************************************************!
SUBROUTINE WRITE_POINTS_3D(mesh_size,x_coord,y_coord,z_coord)
IMPLICIT NONE
!
!...input variables (from previous routine)
!
integer mesh_size
double precision, dimension(mesh_size) :: x_coord,y_coord,z_coord
!
!...local variables
!
integer mm
!
!...open file
!
open(unit=7,file='cd_nozzle_3d.dat',access='APPEND',status='old')
!
!...write current three x, y, and z arrays to a file, append mode
!
do mm = 1,mesh_size
   !
   write(7,101)x_coord(mm),y_coord(mm),z_coord(mm)
   !
end do
!
!...close output file
!
close(7)
!
101 format(f12.6,3x,f12.6,3x,f12.6,3x)
END SUBROUTINE WRITE_POINTS_3D
