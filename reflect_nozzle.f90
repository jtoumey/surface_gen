!*************************************************************************!
!                                                                         !
!  Subroutine:   REFLECT_NOZZLE.f90                                       !
!                                                                         !
!  Programmer:   Julian M. Toumey                                         !
!                Madison, WI                                              !
!                                                                         !
!  Date:         August 2015                                              !
!                                                                         !
!  Language:     FORTRAN 90                                               !
!                                                                         !
!  Description:  This subroutine creates a 2D geometry for a converging-  !
!                diverging nozzle based on an input file with one curve   !
!                for the nozzle surface.                                  !
!                                                                         !
!                Inputs:                                                  !
!                                                                         !
!  Output Files: surface  Output file containing surface coordinates      !
!                                                                         !
!*************************************************************************!
SUBROUTINE REFLECT_NOZZLE(get_input)
IMPLICIT NONE
!
character(len=25  ) :: file_name
character(len=1000) :: buffer
character(len=2   ) :: axis_value
logical get_input,z_write,file_exists
integer ii,jj,num_points,iblnk,mesh_size
double precision reflect_value
double precision, dimension(:), allocatable :: x,z,reflect_points,dummy
double precision, dimension(:), allocatable :: x_coord,y_coord,z_coord
double precision d,theta,angle,PI
!
!...Variable initialization
!
PI = 4.D0 * DATAN(1.D0)
z_write = .FALSE.
!
write(*,*)'ENTER INPUT FILE NAME: '
read(*,*)file_name
!
!...Open and read the file
!
open(unit=2,file = file_name)
write(*,*)'READING INPUT FILE...'
!
!...store the first line to a buffer
!
read(2,'(a)')buffer
!
!...test if the keyword is correct, parse based on tab character, read the number of points
!
if (buffer(1:10) == 'num_points') then
   iblnk = index(buffer,'	')
   read(buffer(iblnk:),*)num_points
!...print error if the first keyword is incorrect, end the program
else
   print *, '!!! FIRST LINE OF FILE MUST CONTAIN KEYWORD num_points, TAB CHARACTER, THEN NUMBER OF POINTS. !!!'
   stop
endif
!
!...allocate memory for the columns in the file
!
allocate(x(num_points),z(num_points),reflect_points(num_points),dummy(num_points))
!
!...loop over lines in file
!
do ii = 1,num_points
   read(2,*)x(ii),z(ii)
   write(*,101)ii,x(ii),z(ii)
end do
close(2)
!
write(*,*)'...FINISHED READING FILE.'
write(*,202)num_points
!
write(*,*)'ENTER REFLECTION AXIS: [x/z axis, reflection value]: '
read(*,*)axis_value,reflect_value
!
!...store the points to be reflected in a dummy array
!
if (axis_value == 'x') then
   dummy = z
   z_write = .TRUE.
else if (axis_value == 'z') then
   dummy = x
end if
!   reflect the points
do ii = 1,num_points
   reflect_points(ii) = 2. * reflect_value - dummy(ii)
end do
!
!...write 2D output to file
!
call write_points_2D(z_write,num_points,x,z,reflect_points)
!
!*************************************************************************!
!                                                                         !
! 3D extrusion of nozzle                                                  !
!                                                                         !
!*************************************************************************!
!
!...test if file exists
!
inquire(file='CD_nozzle_3D.dat',exist=file_exists)
!
if ( file_exists ) then
      !
      !...open with status 'replace' to overwrite
      open(unit=7,file='CD_nozzle_3D.dat',access='APPEND',status='replace')
      close(unit=7)
      !
else
   ! 
   !...open with status 'new' to create a new file if it doesn't exist
   open(unit=7,file='CD_nozzle_3D.dat',access='APPEND',status='new')
   close(unit=7)
   !
endif
!
write(*,*)'ENTER MESH GRANULARITY IN RADIAL DIRECTION: '
read(*,*)mesh_size
!
allocate(x_coord(mesh_size),y_coord(mesh_size),z_coord(mesh_size))
!
theta = 2. * PI / mesh_size
!
!...move axially along C-D nozzle
!
do ii = 1,num_points
   !
   d = abs(z(ii) - reflect_value) ! axis is what points are reflected about
   !
   !...move radially along C-D nozzlenu
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
   call write_points(mesh_size,x_coord,y_coord,z_coord)
   !
end do
!
!...deallocate memory
!
deallocate(x,z)
deallocate(x_coord,y_coord,z_coord)
deallocate(reflect_points,dummy)
!
!...Set input switch, so main routine will exit input loop
!
get_input = .TRUE.
!
!...Format statements
!
101 format(3x,'*** n = ',i4,3x,'x = ',f14.7,3x,'z = ',f14.7,' ***')
202 format(3x,'READ',i4,3x,'DATA POINTS.')
!
END SUBROUTINE REFLECT_NOZZLE
