PROGRAM rootfinder2

  REAL :: a,b,c,y,x,ya,yb,yc
  REAL,PARAMETER :: e = 0.0001 !error
  INTEGER,PARAMETER :: n = 100
  INTEGER :: i

  y(x) = x**5 - (SQRT(3.)*x**3)/2. + 2**(1/3.)
    
  a = -2.
  b = 2.
  DO i=1,n
     c = (a+b)/2.
     ya = y(a)
     yb = y(b)
     yc = y(c)

     IF ((b-a)/2. < e) EXIT
     ! Pseudocode http://mpec.sc.mahidol.ac.th/numer/PSEUDO.HTM
     IF (ya*yc>0) THEN 
        a = c
     ELSE
        b = c
     ENDIF
     WRITE(99,'(2F10.5)') c,yc
  ENDDO

  WRITE(*,'(A,X,F8.4)') 'x0=',c

END PROGRAM rootfinder2 
  
