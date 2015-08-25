
SUBROUTINE WRITE_POINTS_2D(z_write,num_points,x,z,reflect_points)
IMPLICIT NONE
!
!...input variables (from previous routine)
!
logical z_write
integer num_points
double precision, dimension(num_points) :: x,z,reflect_points
!
!...local variables
!
integer ii
!
!...write output to file
!
open(unit=7,file='output_points.dat',action="write",status="replace")
!
!...write original points
!
do ii = 1,num_points
   write(7,301)x(ii),z(ii)
end do
!
if (z_write) then
   do ii = 1,num_points
      write(7,301)x(ii),reflect_points(ii)
   end do
else
   do ii = 1,num_points
      write(7,301)reflect_points(ii),z(ii)
   end do
end if
!
301 format(3x,f12.5,3x,f12.5)
!
END SUBROUTINE WRITE_POINTS_2D
