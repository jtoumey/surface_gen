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
!  Output Files: CD_nozzle_3D.dat   Output file containing x, y, and z    !
!                                   coordinates of points on the nozzle   !
!                                   surface.                              !
!                                                                         !
!*************************************************************************!
SUBROUTINE write_points(num_points,x,z,reflect_value)
IMPLICIT NONE
!
!...Define PI as constant
double precision PI
!
!...input variables (from previous routine)
!
integer num_points
double precision, dimension(num_points) :: x,z
double precision reflect_value
!
!...local variables
!
integer ii,jj,mesh_size
integer mm
double precision, dimension(:), allocatable :: x_coord,y_coord,z_coord
double precision d,theta,angle
!
PI = 4.D0 * DATAN(1.D0)
!
write(*,*)'ENTER MESH GRANULARITY IN RADIAL DIRECTION: '
read(*,*)mesh_size
!
allocate(x_coord(mesh_size),y_coord(mesh_size),z_coord(mesh_size))
!
theta = 2. * PI / mesh_size
!
!   open output file
open(unit=7,file='CD_nozzle_3D.dat',access='APPEND',status='unknown')
!
!...move axially along C-D nozzle
!
do ii = 1,num_points
   !
   d = abs(z(ii) - reflect_value) ! axis is what points are reflected about
   !
   !...move radially along C-D nozzle
   !
   do jj = 1,mesh_size
      !
      x_coord(jj) = x(ii) ! same x-coordinate as in file
      y_coord(jj) = d * sin(theta * (jj-1))
      z_coord(jj) = d * cos(theta * (jj-1)) + reflect_value
      !
   end do
   !
   !...write current three x, y, and z arrays to a file, append mode
   !
   do mm = 1,mesh_size
      write(7,101)x_coord(mm),y_coord(mm),z_coord(mm)
   end do
end do
!
!...close output file
!
close(7)

! call triangulation sub-routine
   ! 1. triangulate intake
   ! 2. triangulate nozzle skin
   ! 3. triangulate outlet


close(7)
!
101 format(f12.6,3x,f12.6,3x,f12.6,3x)
END SUBROUTINE WRITE_POINTS
