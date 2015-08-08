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
IMPLICIT NONE
!
integer n,ii,iblnk
parameter(n=5)

character(len=1000) :: buffer
integer num_points
!
character(len=25) :: file_name
double precision x(n),y(n)
!
!...INPUT SECTION
!
write(*,*)'ENTER INPUT FILE NAME: '
read(*,*)file_name
!
write(*,*)'FILE NAME YOU ENTERED: '
write(*,*)file_name
!
!...
!
open(unit=2,file = file_name)
write(*,*)'READING INPUT FILE...'
!
read(2,'(a)')buffer
print*,buffer(1:10)
!
if (buffer(1:10) == 'num_points') then
   !read(buffer(11:1000),*)num_points
   iblnk = index(buffer,'	')
   read(buffer(iblnk:),*)num_points
   !
   print *,num_points

else
   print *, '!!! FIRST LINE OF FILE MUST CONTAIN KEYWORD num_points, TAB CHARACTER, THEN NUMBER OF POINTS. !!!'
   stop
endif
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
101 format(3x,'*** n = ',i4,3x,'x = ',f14.7,3x,'y = ',f14.7,' ***')
202 format(3x,'READ',i4,3x,'DATA POINTS.')
!
END 
