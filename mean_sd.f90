PROGRAM mean_sd
  USE statistics
  
  IMPLICIT NONE

  REAL, ALLOCATABLE :: a(:)
  REAL :: m, sigma
  INTEGER :: n

  WRITE(*,*) "how many numbers do you want to type in ?"
  READ(*,*) n
  ALLOCATE(a(n))
  WRITE(*,*) "please type in the numbers"
  READ(*,*) a
  
  CALL mean(n,a,m)
  CALL sd(n,a,m,sigma)

  DEALLOCATE(a)
  
END PROGRAM mean_sd
  
MODULE statistics
  CONTAINS
    
  SUBROUTINE mean(n,a,m)
    REAL, ALLOCATABLE :: a(:)
    REAL :: m
    INTEGER :: n
    
    m = (1/REAL(n))*SUM(a)
    WRITE(*,*) m
    
  END SUBROUTINE mean

  SUBROUTINE sd(n,a,m,sigma)
    REAL, ALLOCATABLE :: a(:)
    REAL :: m, sigma
    INTEGER :: n
    sigma = SQRT((1/REAL(n))*SUM((a-m)**2))
    WRITE(*,*) sigma

  END SUBROUTINE sd
  
END MODULE statistics


