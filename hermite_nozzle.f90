!*************************************************************************!
!                                                                         !
!  Subroutine:   HERMITE_NOZZLE.f90                                       !
!                                                                         !
!  Programmer:   Julian M. Toumey                                         !
!                Madison, WI                                              !
!                                                                         !
!  Date:         August 2015                                              !
!                                                                         !
!  Language:     FORTRAN 90                                               !
!                                                                         !
!  Description:  This subroutine creates a 2D geometry for a converging-  !
!                diverging nozzle using Hermite interpolation between     !
!                specific points in a C-D nozzle.                         !
!                                                                         !
!                Inputs:                                                  !
!                   get_input   control for input loop in main routine    !
!                                                                         !
!  Output Files: surface  Output file containing surface coordinates      !
!                                                                         !
!*************************************************************************!
SUBROUTINE HERMITE_NOZZLE(get_input)
IMPLICIT NONE
!

!
!
!...Set input switch, so main routine will exit input loop
!
get_input = .TRUE.
!
END SUBROUTINE HERMITE_NOZZLE
