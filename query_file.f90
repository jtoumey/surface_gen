SUBROUTINE QUERY_FILE(file_exists)
IMPLICIT NONE
!
!...input variables from previous subroutine
!
logical file_exists
!
inquire(file='cd_nozzle_3d.dat',exist=file_exists)
!
if ( file_exists ) then
      !
      !...open with status 'replace' to overwrite
      open(unit=7,file='cd_nozzle_3d.dat',access='APPEND',status='replace')
      close(unit=7)
      !
else
   ! 
   !...open with status 'new' to create a new file if it doesn't exist
   open(unit=7,file='cd_nozzle_3d.dat',access='APPEND',status='new')
   close(unit=7)
   !
endif
!
END SUBROUTINE QUERY_FILE
