program interp
implicit none
!
integer ii,jj,n
double precision, dimension(4) :: x,y
double precision p,xp,phi
!
n = 4
x = (/ 3.2, 2.7 , 1.  , 4.8  /)
y = (/ 22., 17.8, 14.2, 38.3 /)
!
xp = 3.5
!
do ii = 1,n
   phi = 1.
   do jj = 1,n
   if (ii .eq. jj) cycle
   !
   phi = phi * (xp - x(jj)) / (x(ii) - x(jj))
   !
   end do
p = p + phi*y(ii)
   !write(*,*)ii,jj,p
end do 
!
write(*,*)p
!
end program interp
