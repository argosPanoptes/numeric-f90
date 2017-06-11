! numerical integration using trapezoidal method to find circumference of an ellipse

MODULE ellipse

CONTAINS
  
  FUNCTION f1(x,a,b)
    REAL :: f1,x,a,b
    f1 = (((a * SIN(x))**2) + ((b * COS(x))**2))
  END FUNCTION f1

END MODULE ellipse

MODULE trapezoid

CONTAINS
  
  SUBROUTINE integrate(f, from, to, a, b, It, n)

    INTEGER :: n,i
    REAL :: It, h , s1, s2, a, b, from, to
    
    INTERFACE
       FUNCTION f(x,a,b)
         REAL :: f,x,a,b
       END FUNCTION f
    END INTERFACE

    h = (to - from) / n
    s =  f(to,a,b) + f(from,a,b)
    DO i=1,n
       s = s + (2. * f(from + i * h, a,b))
    ENDDO

    It = h * s/2.
   
    WRITE(*,*) It

  END SUBROUTINE integrate

END MODULE trapezoid

PROGRAM circumference

  USE ellipse
  USE trapezoid
    
  IMPLICIT NONE

  INTEGER :: n
  REAL :: a, b, from, to, twopi, It

  n=100
  twopi = 2. * ACOS(-1.0)

  WRITE(*,FMT="(A)",ADVANCE="no") "length of  major axis: "
  READ(*,*)a
  WRITE(*,FMT="(A)",ADVANCE="no") "length of minor axis: "
  READ(*,*)b
  
  CALL integrate(f1, 0., twopi, a, b, It, n)

END PROGRAM circumference
