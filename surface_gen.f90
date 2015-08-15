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
logical get_input
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
!...Input loop
!
do while (.NOT. get_input)
   !
   read(*,*)input_mode
   !
   if (input_mode == 1) then
      !
      call reflect_nozzle(get_input)
      !
   else if (input_mode == 2) then
      !
      call hermite_nozzle(get_input)
      !
   else if (input_mode == 3) then
      !

      !
   else 
      write(*,*)'!!! INVALID INPUT SELECTION. PLEASE CHOOSE (1), (2), OR (3) !!!'
   end if
   !
end do
!
write(*,*)'FINISHED GENERATING OUTPUT FILE output_points.dat.'
write(*,*)'DONE.'
!
END 
