MODULE seed

CONTAINS

  SUBROUTINE init_random_seed() !https://stackoverflow.com/questions/18754438/generating-random-numbers-in-a-fortran-module

    INTEGER :: i, n, clock
    INTEGER, DIMENSION(:), ALLOCATABLE :: seed
    
    CALL RANDOM_SEED(size = n)
    ALLOCATE(seed(n))

    CALL SYSTEM_CLOCK(COUNT=clock)

    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    CALL RANDOM_SEED(PUT = seed)

    DEALLOCATE(seed)
  END SUBROUTINE init_random_seed

END MODULE seed

PROGRAM mcint

  USE seed
  
  IMPLICIT NONE

  INTEGER :: Npoints, Ntries, j, k, l
  INTEGER, PARAMETER :: p=8
  INTEGER, PARAMETER :: dim=4
  REAL(p), ALLOCATABLE :: r(:)
  REAL(p), ALLOCATABLE :: I(:)
  REAL(p) :: b,a, vol_min, vol_max, vol_ave, vol_var, ns, dotp, min, jfound, max

  CALL init_random_seed()
  
  READ(*,*) Npoints, Ntries

  ALLOCATE(r(dim))
  ALLOCATE(I(Ntries))

  vol_min = I(1)
  vol_max = I(1)
  
  DO j=1,Ntries
     r = 0
     ns = 0
     DO k=1,Npoints
        CALL RANDOM_NUMBER(r)
        dotp = DOT_PRODUCT(r,r)
        IF (dotp < 1.) THEN
           ns = ns + 1
        ENDIF
     ENDDO
     I = (ns/Npoints)*2**dim
     
     IF (I(j) < vol_min) vol_min = I(j)
     IF (I(j) > vol_max) vol_max = I(j)
     
  ENDDO

  !vol_min = MINVAL(I)
  !vol_max = MAXVAL(I)
  vol_ave = SUM(I)/Ntries
  !vol_var = 1./REAL(Ntries) * SUM((I(1:Ntries)-vol_ave)**2)
  vol_var =  ((1/REAL(Ntries)) * SUM(I(1:Ntries)**2)) - vol_ave**2
  
  WRITE(*,'(2I8,4F14.7)') Npoints,Ntries,vol_ave,vol_var,vol_min,vol_max
     
  DEALLOCATE(r)
  DEALLOCATE(I)
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

  
