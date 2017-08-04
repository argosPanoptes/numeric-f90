PROGRAM mcint

  IMPLICIT NONE

  INTEGER :: Npoints, Ntries, i, j, ns
  INTEGER, PARAMETER :: p=8
  INTEGER, PARAMETER :: dim=4
  REAL, ALLOCATABLE :: r(:)
  REAL(p) :: I,b,a

  READ(*,*) Npoints, Ntries

  ALLOCATE(r(dim))
  
  ns=0
  DO i=1,Ntries
     DO j=1,Npoints
        CALL RANDOM_NUMBER(r)
  
  
  !N, parameter = total # points
  !I = (b-a) n_s/N *H
  ! 4 dimensional allocatable array
  !read in N points, N tries
  !double DO LOOP
  !n_s = 0
  !DO NT=1,NTries
  !DO NP = 1,N
  !CALL RANDOM_NUMBER(r)
  !check whether random number is inside or outside with help of function
  !IF (inside(r)) THEN n_s = n_s + 1
  !ENDDO
  ! I = n_s/N*2*NDIM (=4) = 1/2 pi^2 (in 4 dim); 4/3 pi (in 3 dim)
  ! Imin = MIN(...)
  ! Imax = MAX(...)
  !ENDDO
  !Iaverage
  !DO LOOP variance
  
END PROGRAM mcint

  
