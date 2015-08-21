
SUBROUTINE write_points(num_points,x,z)
IMPLICIT NONE
!
!...Define PI as constant
double precision PI
!
!...input variables (from previous routine)
!
integer num_points
double precision, dimension(num_points) :: x,z
!
!...local variables
!
integer ii,jj,mesh_size
integer mm,nn
double precision, dimension(:), allocatable :: x_coord,y_coord,z_coord
double precision d,theta,angle
!

PI = 4.D0 * DATAN(1.D0)
!x(1) = ! x(1) from file, probably 0
!y(1) = 0. !
!z(1) = ! z(1) from file, first radial point on C-D nozzle

!write(*,*)'ENTER MESH GRANULARITY IN RADIAL DIRECTION: '
!read(*,*)mesh_size
mesh_size = 12
!
allocate(x_coord(mesh_size),y_coord(mesh_size),z_coord(mesh_size))
!
theta = 2. * PI / mesh_size
!
!...move axially along C-D nozzle
!
!
do ii = 1,num_points
   !
   d = z(ii) ! axis is what you reflect about
   !
   !...move radially along C-D nozzle
   !
   do jj = 1,mesh_size
      x_coord(jj) = x(ii) ! same x-coordinate as in file
      y_coord(jj) = d * sin(theta * (jj-1))
      z_coord(jj) = d * cos(theta * (jj-1))
   end do
   
   ! write current three x, y, and z arrays to a file, append mode
   open(unit=7,file='CD_nozzle_2D.dat',access='APPEND',status='old')
   do nn = 1,mesh_size
      write(7,101)x_coord(nn),y_coord(nn),z_coord(nn)
   end do
   close(7)
end do


! call triangulation sub-routine
   ! 1. triangulate intake
   ! 2. triangulate nozzle skin
   ! 3. triangulate outlet


close(7)

101 format(f12.6,3x,f12.6,3x,f12.6,3x)
END SUBROUTINE WRITE_POINTS
