PROGRAM QUADRATIC

  IMPLICIT NONE
  
  REAL :: a, b, c, r1, r2, arg 
  COMPLEX :: c1, c2
  WRITE(*,*) "Quadratic Equation Solver. Please type in a, b, c"
  READ(*,*) a,b,c

  IF (a == 0. .AND. b /= 0.) THEN
     WRITE(*,*) "x is ", -c/b
  ELSE 
     arg = b**2 - 4.*a*c
     IF (arg < 0.) THEN
        WRITE(*,*) "There are two complex solutions:" ! Ansatz aus 'Metcalf - Fortran 90 Explained' Vermeidung NaN
        WRITE(*,*) -b/(2.*a), '+-', SQRT(-arg)/(2.*a) 
     ELSE
        r1 = (-b + SQRT(arg))/(2.*a)
        r2 = (-b - SQRT(arg))/(2.*a)
        WRITE(*,*) "There are two real solutions:"
        WRITE(*,'(A,X,F10.5)') "x1=", r1
        WRITE(*,'(A,X,F10.5)') "x2=", r2
     ENDIF
  ENDIF
    
END PROGRAM QUADRATIC
