PROGRAM FACTORIAL

  IMPLICIT NONE

  INTEGER :: n, i, nfactorial
  WRITE(*,*) "give a positive integer n to compute n!"
  READ(*,*) n ! read in a number  
  nfactorial = PRODUCT((/(i, i=1,n)/)) ! http://rosettacode.org/wiki/Factorial#Fortran_90
  WRITE(*,*) nfactorial

END PROGRAM FACTORIAL

