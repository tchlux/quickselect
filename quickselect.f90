MODULE QUICKSELECT
  USE ISO_FORTRAN_ENV, ONLY : REAL64, INT64
  IMPLICIT NONE

CONTAINS

  ! Sort a given array with the bubble sort method.
  SUBROUTINE BUBBLE_SORT(VALUES)
    REAL(KIND=REAL64), DIMENSION(:), INTENT(INOUT) :: VALUES
    ! Local variables
    REAL(KIND=REAL64) :: TEMP
    INTEGER :: I, J, K
    OUTER : DO I = SIZE(VALUES), 2, -1
       INNER : DO J = I, SIZE(VALUES)
          K = J-1
          IF (VALUES(K) .LE. VALUES(J)) EXIT INNER
          TEMP = VALUES(K)
          VALUES(K) = VALUES(J)
          VALUES(J) = TEMP
       END DO INNER
    END DO OUTER
  END SUBROUTINE BUBBLE_SORT

  ! Return the median-of-medians index for a list of values. Identify
  ! the index of a value such that 30th percentile <= value <= 70th percentile
  RECURSIVE FUNCTION MEDIAN_OF_MEDIANS(VALUES, GROUP_SIZE) RESULT(INDEX)
    REAL(KIND=REAL64), DIMENSION(:), INTENT(INOUT) :: VALUES
    INTEGER, INTENT(IN), OPTIONAL :: GROUP_SIZE
    INTEGER :: INDEX
    ! Local variables.
    INTEGER :: GS, MID, I, LOWER, UPPER
    REAL(KIND=REAL64) :: TEMP
    ! Automatically set the group size to 5 if not provided.
    IF (PRESENT(GROUP_SIZE)) THEN; GS = GROUP_SIZE
    ELSE;                          GS = 5;
    END IF
    ! Check for base case, sort the group, return the true median.
    IF (SIZE(VALUES) .LE. GS) THEN;
       CALL BUBBLE_SORT(VALUES)
       INDEX = (1 + SIZE(VALUES)) / 2
    ! Otherwise, compute true (group-size) medians for all values.
    ELSE
       ! Loop through all groups (including last not-full dangling group).
       DO I = 0, (SIZE(VALUES) - 1) / GS
          ! Compute the lower and upper indices of this group.
          LOWER = GS*I
          UPPER = MIN(LOWER + GS, SIZE(VALUES))
          LOWER = LOWER + 1
          ! Sort this group, get the true median.
          CALL BUBBLE_SORT(VALUES(LOWER : UPPER))
          MID = (LOWER + UPPER) / 2
          ! Swap the true median up to the front of the list of values.
          TEMP = VALUES( MID )
          VALUES(MID) = VALUES(I+1)
          VALUES(I+1) = TEMP
       END DO
       ! Update estimated median index (will be stored in the front part of VALUES).
       INDEX = I / 2
       ! Recursively call 'select' to find the median of the medians.
       CALL SELECTR(VALUES(:I-1), INDEX)
    END IF
  END FUNCTION MEDIAN_OF_MEDIANS

  ! Partition a list of "VALUES" about the number at index "PIVOT".
  ! Identify which side of "PIVOT" the "INDEX" value will be, return
  ! the "LOC"ation of an index bound on the subset of "VALUES" that
  ! will contain "INDEX" if "VALUES" were perfectly sorted.
  ! 
  ! When "INDEX" is not provided, this function will partition values
  ! as one would normally expect (all values up to "PIVOT" will be
  ! less or equal, all values after will be greater) in place.
  FUNCTION PARTITION(VALUES, PIVOT, INDEX) RESULT(LOC)
    REAL(KIND=REAL64), DIMENSION(:), INTENT(INOUT) :: VALUES
    INTEGER, INTENT(IN) :: PIVOT
    INTEGER, INTENT(IN), OPTIONAL :: INDEX
    INTEGER :: LOC
    ! Local variables
    REAL(KIND=REAL64) :: TEMP, PIVOT_VALUE
    INTEGER :: I, FREE, LOCAL_INDEX
    ! Check for the presence of "INDEX".
    IF (PRESENT(INDEX)) THEN ; LOCAL_INDEX = INDEX
    ELSE ;                     LOCAL_INDEX = SIZE(VALUES)
    END IF
    ! Start the counter of storage for swapped elements.
    FREE = 1
    ! Swap the pivot element to the back of the list.
    PIVOT_VALUE = VALUES(PIVOT)
    VALUES(PIVOT) = VALUES(SIZE(VALUES))
    VALUES(SIZE(VALUES)) = PIVOT_VALUE
    ! Partition the values that are less than the pivot.
    DO I = 1, SIZE(VALUES)-1
       IF (VALUES(I) .LT. PIVOT_VALUE) THEN
          TEMP = VALUES(FREE)
          VALUES(FREE) = VALUES(I)
          VALUES(I) = TEMP
          FREE = FREE + 1
       END IF
    END DO
    ! Check to see if we have already found which half contains "INDEX".
    IF (LOCAL_INDEX .LT. FREE) THEN
       LOC = FREE - 1
       ! Swap the pivot value back into its proper place and return lower bound.
       VALUES(SIZE(VALUES)) = VALUES(FREE)
       VALUES(FREE) = PIVOT_VALUE
       RETURN
    END IF
    ! Partition values that are equal to the pivot.
    DO I = 1, SIZE(VALUES)-1
       IF (VALUES(I) .EQ. PIVOT_VALUE) THEN
          TEMP = VALUES(FREE)
          VALUES(FREE) = VALUES(I)
          VALUES(I) = TEMP
          FREE = FREE + 1
       END IF
    END DO
    ! Swap the pivot value back into its proper place and return lower bound.
    VALUES(SIZE(VALUES)) = VALUES(FREE)
    VALUES(FREE) = PIVOT_VALUE
    ! Check to see if we have found "INDEX" inside the equality block,
    ! return the exact index (since it is in its correct location).
    IF (LOCAL_INDEX .LE. FREE) THEN
       LOC = LOCAL_INDEX
    ! Otherwise, the location of index is somewhere to the right of
    ! the final location that PIVOT is placed at.
    ELSE
       LOC = FREE
    END IF
  END FUNCTION PARTITION

  ! Given an unordered list of values, sort the values such that the
  ! element at VALUES(INDEX) has rank INDEX, such that all preceding
  ! numbers are smaller or equal and all following numbers are greater
  ! or equal.
  ! 
  ! The "R" at the end is for 'recursive', because this is a recursive
  ! implementation of the select method. This method also does not
  ! implement the optimal average case optimizations, it *always*
  ! identifies a pivot value via the median-of-medians method.
  RECURSIVE SUBROUTINE SELECTR(VALUES, INDEX)
    REAL(KIND=REAL64), DIMENSION(:), INTENT(INOUT) :: VALUES    
    INTEGER, INTENT(IN) :: INDEX
    ! Local variables
    INTEGER :: PIVOT, I
    REAL(KIND=REAL64) :: PIVOT_VALUE
    IF (SIZE(VALUES) .EQ. 1) RETURN
    PIVOT = MEDIAN_OF_MEDIANS(VALUES)
    ! PIVOT = SIZE(VALUES) / 2
    PIVOT_VALUE = VALUES(PIVOT)
    I = PARTITION(VALUES, PIVOT, INDEX)
    ! Recurse, calling SELECT on the half of VALUES that contains INDEX.
    IF ((I .EQ. INDEX) .AND. (VALUES(I) .EQ. PIVOT_VALUE)) THEN ; RETURN
    ELSE IF (I .LT. INDEX) THEN ; CALL SELECTR(VALUES(I+1:), INDEX-I)
    ELSE                        ; CALL SELECTR(VALUES(:I), INDEX)
    END IF
  END SUBROUTINE SELECTR

END MODULE QUICKSELECT
