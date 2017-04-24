PROGRAM FOURIER

  REAL :: a, b, c, t=0.0
  REAL, PARAMETER :: pi = ACOS(-1.0)

  DO
     t = t + 0.001 
     IF (t>7) EXIT
     a = (8/(pi**2))*SIN(2.0*t)
     b = (8/(pi**2))*(SIN(2.0*t)-(1/9.)*SIN(6.0*t))
     c = (8/(pi**2))*(SIN(2.0*t)-(1/9.)*SIN(6.0*t)+(1/25.)*SIN(10.0*t))
     WRITE(*,'(4(F9.6,X))') t,a,b,c
  ENDDO

END PROGRAM FOURIER
