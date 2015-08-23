SUBROUTINE HERMITE_NOZZLE(get_input)



logical get_input

double precision a,b,c,d,e,a1,a2


write(*,*)'ENTER A,B,C,D,E,A1,A2: '
read(*,*)a,b,c,d,e,a1,a2



!write(*,*)'HERMITE NOZZLE TEST'

get_input = .TRUE.


END SUBROUTINE HERMITE_NOZZLE
