PROGRAM differentiation

  IMPLICIT NONE

  INTEGER :: i
  INTEGER, PARAMETER :: p=8
  INTEGER, PARAMETER :: n=10000
  REAL(p), PARAMETER :: twopi = (2.*ACOS(-1.0))
  REAL(p), PARAMETER :: deltax = 0.01
  REAL(p), PARAMETER :: steps = twopi/n
  REAL(p) :: x, dcosx, cosx, sinx, error, minx

  x = 0.
  
  DO i=1,n
     x = x + steps*i
     cosx = COS(x)
     sinx = SIN(x)
     minx = -1.*sinx
     dcosx = (COS(x+deltax)-COS(x-deltax))/(2*deltax)
     error = dcosx + SIN(x)
     OPEN(UNIT=10, FILE="out11.dat", ACTION="WRITE")
     WRITE(10,"(5(F9.6,X))") x, cosx, dcosx, minx, error
     IF (x > twopi) EXIT
  ENDDO
  
  ! WRITE(10,"(5(F9.6,X)") x, cosx, d cosx/dx, -1sinx, error
  ! 5 numbers 9 digits float, 6 of them after comma
  ! x el [0,2pi] divide this interval in e.g. 100 points, Loop over points
  ! for each point cosx, dcosx, -1 * sinx, error
  ! calculate derivative of cosx numerically:
  ! d cos(x) / dx = (cos(x + delta x)-cos(x- delta x))/delta x
  ! error = dcosx + sinx
  ! error*10^4

END PROGRAM differentiation
