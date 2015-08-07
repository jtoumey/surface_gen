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
integer n,ii
parameter(n=5)
!
character(len=25) :: file_name
double precision x(n),y(n)
!
!...INPUT SECTION
!
write(*,*)'ENTER INPUT FILE NAME: '
read(*,*)file_name


write(*,*)'YOU ENTERED: '
write(*,*)file_name

!
!...
!
open(unit=2,file = file_name)
write(*,*)'READING INPUT FILE...'
!...loop over lines in file
!
do ii = 1,n
   read(2,*)x(ii),y(ii)
   write(*,101)ii,x(ii),y(ii)
end do
close(2)
!
write(*,*)'...FINISHED READING FILE.'
write(*,202)n
!
101 format(3x,'*** n = ',i4,3x,'x = ',f14.7,3x,'y = ',f14.7,' ***')
202 format(3x,'READ',i4,3x,'DATA POINTS.')
!
END 
