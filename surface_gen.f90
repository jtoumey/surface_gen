!*************************************************************************!
!                                                                         !
!  File:         surface_gen.f90                                          !
!                                                                         !
!  Programmer:   Julian M. Toumey                                         !
!                Madison, WI                                              !
!                                                                         !
!  Date:         August 2015                                              !
!                                                                         !
!  Language:     FORTRAN 90                                               !
!                                                                         !
!  Description:  This module generates an *.stl surface file based on     !
!                user inputs.                                             !
!                                                                         !
!  Output Files: surface  Output file containing surface coordinates      !
!                                                                         !
!*************************************************************************!
PROGRAM SURFACE_GEN
!
IMPLICIT NONE
!
integer ii,iblnk,iblnk2
integer num_points
!
character(len=1000) :: buffer,buf_reflect
character(len=25  ) :: file_name
character(len=2   ) :: axis_value
double precision, dimension(:), allocatable :: x,y,reflect_points,dummy
integer reflect_value
!
!...INPUT SECTION
!
write(*,*)'ENTER INPUT FILE NAME: '
read(*,*)file_name
!
!...Open and read the file
!
open(unit=2,file = file_name)
write(*,*)'READING INPUT FILE...'

!   store the first line to a buffer

read(2,'(a)')buffer

!   test if the keyword is correct, parse based on tab character, read the number of points

if (buffer(1:10) == 'num_points') then
   iblnk = index(buffer,'	')
   read(buffer(iblnk:),*)num_points
!   print error if the first keyword is incorrect, end the program
else
   print *, '!!! FIRST LINE OF FILE MUST CONTAIN KEYWORD num_points, TAB CHARACTER, THEN NUMBER OF POINTS. !!!'
   stop
endif
!
!...allocate memory for the columns in the file
!
allocate(x(num_points),y(num_points),reflect_points(num_points),dummy(num_points))
!
!...loop over lines in file
!
do ii = 1,num_points
   read(2,*)x(ii),y(ii)
   write(*,101)ii,x(ii),y(ii)
end do
close(2)
!
write(*,*)'...FINISHED READING FILE.'
write(*,202)num_points
!
write(*,*)'ENTER REFLECTION AXIS: [x/y axis, reflection value]: '
read(*,*)axis_value,reflect_value
!
!...store the points to be reflected in a dummy array
!
if (axis_value == 'x') then
   dummy = y
elseif (axis_value == 'y') then
   dummy = x
endif
!   reflect the points
do ii = 1,num_points
   reflect_points(ii) = 2. * reflect_value - dummy(ii)
end do
!   test
write(*,*)reflect_points
!













!
!...deallocate memory
!
deallocate(x,y)
deallocate(reflect_points)
!
101 format(3x,'*** n = ',i4,3x,'x = ',f14.7,3x,'y = ',f14.7,' ***')
202 format(3x,'READ',i4,3x,'DATA POINTS.')
!
END 
