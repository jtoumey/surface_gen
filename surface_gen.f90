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
integer input_mode
integer ii,iblnk,iblnk2
integer num_points
logical x_write,y_write,get_input
!
double precision, dimension(:), allocatable :: x,y,reflect_points,dummy
integer reflect_value
!
!...Initialization
!
get_input = .FALSE.
!
!...INPUT SECTION
!
write(*,*),'CHOOSE NOZZLE CREATION METHOD: '
write(*,*),'   1. SUPPLY INPUT FILE'
write(*,*),'   2. USE HERMITE INTERPOLATION'
write(*,*),'   3. USE CUBIC SPLINES'
!
!
!
do while (.NOT. get_input)
   read(*,*)input_mode
   !
   if (input_mode == 1) then
      !
      call reflect_nozzle(get_input)
      !
   else if (input_mode == 2) then
   !
   else if (input_mode == 3) then
   !
   else 
      write(*,*)'!!! INVALID INPUT SELECTION. PLEASE CHOOSE (1), (2), OR (3) !!!'
   end if
   !
end do





!
END 
