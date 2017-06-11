MODULE function

  CONTAINS

  FUNCTION f1(x) 
    REAL :: f,x
    f = COS(x**3 - x)
  END FUNCTION f1

END MODULE function

MODULE integration

  CONTAINS
  
  FUNCTION simpson(f, a, b, Is, n) !pseudocode http://rosettacode.org/wiki/Numerical_integration

    INTEGER :: n
    INTEGER :: i
    REAL :: Is
    REAL :: h , s1, s2, s
    REAL :: a, b
    REAL :: simpson
    
    INTERFACE
       FUNCTION f(x)
         REAL :: f
         REAL :: x
       END FUNCTION f
    END INTERFACE
    
    
    h = (b - a)/n
    s1 = f(a + h/2.)
    s2 = 0.
    
    DO i=1,n-1
       s1 = s1 + f(a + h * i + h/2.)
       s2 = s2 + f(a + h * i)
    ENDDO

    Is = (h / 6.) * (f(a) + f(b) + 4.*s1 + 2.*s2)
     
  END FUNCTION simpson
  
  FUNCTION trapezoidal (f, a, b, It, n) 
    
    INTEGER :: n
    INTEGER :: i
    REAL :: It
    REAL :: h , s, a, b
    REAL :: trapezoidal

    INTERFACE
       FUNCTION f(x)
         REAL :: f
         REAL :: x
       END FUNCTION f
    END INTERFACE

    h = (b - a) / n
    s =  f(a) + f(b)
    DO i=1,n
       s = s + (2. * f(a + i *h))
    ENDDO

    It = h * s/2.
    
  END FUNCTION trapezoidal
    
END MODULE integration

PROGRAM numint

  USE function
  USE integration
  
  IMPLICIT NONE
 
  INTEGER :: n, i
  REAL :: Is, It, a, b

  WRITE(*,*) simpson(f1, 0., 1., Is, 100)
  WRITE(*,*) trapezoidal(f1, 0., 1., It, 100)
  
END PROGRAM numint

!Simpson pseudocode
!h := (b - a) / n
!sum1 := f(a + h/2)
!sum2 := 0

!loop on i from 1 to (n - 1)
!sum1 := sum1 + f(a + h * i + h/2)
!sum2 := sum2 + f(a + h * i)

!answer := (h / 6) * (f(a) + f(b) + 4*sum1 + 2*sum2)

! trapezoidal pseudocode
!double h = (to - from) / n;
!double sum = func(from) + func(to);
!int i;
!for(i = 1;i < n;i++)
!sum += 2.0*func(from + i * h);
!return  h * sum / 2.0;
