PROGRAM mandelbrot
  USE set

  IMPLICIT NONE
  
  COMPLEX :: z, c
  REAL :: x,y
  INTEGER :: i, j, k
  INTEGER, PARAMETER :: max=1000
  
  DO i=1,max
     x= -2.
     x = x + 0.005*i
     IF (x >= 1) THEN
        EXIT
     ENDIF
     DO j=1,max
        y = -1.5
        y = y + 0.005*j
        IF (y >= 1.5) THEN
           EXIT
        ENDIF
           IF (inside_set(x,y) .EQV. .TRUE.) THEN
              WRITE (1,"(2(F10.6,X))") x, y
           ENDIF
     ENDDO
  ENDDO

END PROGRAM mandelbrot

MODULE set
CONTAINS
  
  LOGICAL FUNCTION inside_set(x,y)
    REAL :: x,y
    COMPLEX :: c, z
    INTEGER :: i
    INTEGER, PARAMETER :: max=1000
    
    c = CMPLX(x,y)
    z = 0
    inside_set = .TRUE.
    DO i=0,max
       z = z**4 + c
       IF (ABS(z) > 2) THEN
          inside_set = .FALSE.
       ENDIF
    ENDDO    
  END FUNCTION inside_set
END MODULE set

    
  

! c = x+iy
! z_n+1 = (z_n)^4 + c
! konvergiert z_n für c (x,y) in der Mandelbrotmenge |z_n|>2 exit
! konvergenzkriterium n =1000 DO i=1,n |z_n|<2

! complex number zn
! pool of zn
! z_(n+1) = z_(n)^4+c -> converge z_n+1
! dense grid of points, for each point(x,y) check if series converges or not
! if series converges save point (x,y), if not, neglect the point
! DO loop along x, DO loop along y ; for each point another DO loop to check convergence
! LOGICAL FUNCTIONinside_set(x,y) set .false. if abs(z) set true blablashit
