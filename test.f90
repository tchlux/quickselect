PROGRAM TEST
  USE ISO_FORTRAN_ENV, ONLY: REAL64
  USE QUICKSELECT
  IMPLICIT NONE

  ! Initialize a random array of numbers
  REAL(KIND=REAL64) :: VALUES(100)
  CALL RANDOM_NUMBER(VALUES)

  ! Test a subroutine.
  CALL SELECT(VALUES, (SIZE(VALUES)+1) / 2)
  
END PROGRAM TEST

  ! ! Show some header information.
  ! PRINT *, ''
  ! PRINT *, 'Running test program..'
  ! PRINT *, 'VALUES(:3)  ', VALUES(:5)
  ! PRINT *, 'VALUES(-5:) ', VALUES(SIZE(VALUES)-4:)
  ! PRINT *, ''


  ! CALL BUBBLE_SORT(VALUES)
  ! PRINT *, ''
  ! PRINT *, 'VALUES(:5)  ', VALUES(:5)
  ! PRINT *, 'VALUES(-5:) ', VALUES(SIZE(VALUES)-4:)
  ! PRINT *, 'MINVAL(VALUES): ', MINVAL(VALUES)
  ! PRINT *, 'MAXVAL(VALUES): ', MAXVAL(VALUES)
