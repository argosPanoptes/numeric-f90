PROGRAM EASTER ! http://wikipedia.qwika.com/de2en/Gau%C3%9Fsche_Osterformel

  IMPLICIT NONE

  INTEGER :: a, b, c, M, N, H1, H2, d, e, y, O
  WRITE(*,*) "calculate the date of easter for a given year y"
  READ(*,*) y ! read in year
  a = MOD(y, 19)
  b = MOD(y, 4)
  c = MOD(y, 7)
  H1 = y / 100
  H2 = y / 400
  N = 4 + H1 - H2
  M = 15 + H1 - H2 - ((8*H1 + 13) / 25)
  d = MOD((19*a + M), 30)
  e = MOD((2*b + 4*c + 6*d + N), 7)
  
  IF (d+e == 35) THEN
     O = 35
  ELSEIF (d==28 .AND. e==6 .AND. a>10) THEN
     O = 49
  ELSE 
      O = 22 + d + e
  ENDIF
  

  IF (O>31) THEN
     O = O-31
     WRITE(*,*) "Year:", y
     WRITE(*,*) "Month:       April"
     WRITE(*,*) "Day:", O
  ELSEIF (O<31) THEN
     O = O
     WRITE(*,*) "Year:", y
     WRITE(*,*) "Month:       March"
     WRITE(*,*) "Day:", O
  ENDIF

  
END PROGRAM EASTER
