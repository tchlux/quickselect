MODULE FAST_SELECT
  USE ISO_FORTRAN_ENV, ONLY : REAL64, INT64
  IMPLICIT NONE

CONTAINS

  ! ------------------------------------------------------------------
  !                Introselect and associated functions
  ! ------------------------------------------------------------------

  ! Short subroutine for swapping out two values.
  SUBROUTINE SWAP(V1, V2)
    REAL(KIND=REAL64), INTENT(INOUT) :: V1, V2
    ! Local temp
    REAL(KIND=REAL64) :: TEMP
    TEMP = V1
    V1 = V2
    V2 = TEMP
  END SUBROUTINE SWAP

  ! Sort a given array with the bubble sort method.
  SUBROUTINE BUBBLE_SORT(VALUES)
    REAL(KIND=REAL64), DIMENSION(:), INTENT(INOUT) :: VALUES
    ! Local variables
    INTEGER :: I, J, K
    OUTER : DO I = SIZE(VALUES), 2, -1
       INNER : DO J = I, SIZE(VALUES)
          K = J-1
          IF (VALUES(K) .LE. VALUES(J)) EXIT INNER
          CALL SWAP(VALUES(K), VALUES(J))
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
          CALL SWAP(VALUES(MID), VALUES(I+1))
       END DO
       ! Update estimated median index (will be stored in the front part of VALUES).
       INDEX = I / 2
       ! Recursively call 'select' to find the median of the medians.
       CALL INTROSELECT(VALUES(:I-1), INDEX)
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
  ! 
  ! This version groups together values equal to the pivot.
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
    CALL SWAP(VALUES(PIVOT), VALUES(SIZE(VALUES)))
    ! Partition the values that are less than the pivot.
    DO I = 1, SIZE(VALUES)-1
       IF (VALUES(I) .LT. PIVOT_VALUE) THEN
          CALL SWAP(VALUES(FREE), VALUES(I))
          FREE = FREE + 1
       END IF
    END DO
    ! Check to see if we have already found which half contains "INDEX".
    IF (LOCAL_INDEX .LT. FREE) THEN
       LOC = FREE - 1
       ! Swap the pivot value back into its proper place and return lower bound.
       CALL SWAP(VALUES(FREE), VALUES(SIZE(VALUES)))
       RETURN
    END IF
    ! Partition values that are equal to the pivot.
    DO I = FREE, SIZE(VALUES)-1
       IF (VALUES(I) .EQ. PIVOT_VALUE) THEN
          CALL SWAP(VALUES(FREE), VALUES(I))
          FREE = FREE + 1
       END IF
    END DO
    ! Swap the pivot value back into its proper place and return lower bound.
    CALL SWAP(VALUES(FREE), VALUES(SIZE(VALUES)))
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
  SUBROUTINE INTROSELECT(VALUES, INDEX, ALLOWANCE)
    REAL(KIND=REAL64), DIMENSION(:), INTENT(INOUT) :: VALUES    
    INTEGER, INTENT(IN) :: INDEX
    REAL(KIND=REAL64), INTENT(IN), OPTIONAL :: ALLOWANCE
    ! Local variables
    INTEGER :: PIVOT, I, SEARCHED, FIRST, LAST
    REAL(KIND=REAL64) :: PIVOT_VALUE, LOCAL_ALLOWANCE
    LOGICAL :: RANDOM_PIVOT
    ! Handle assignment of 'allowance' for switch to median-of-medians.
    IF (PRESENT(ALLOWANCE)) THEN ; LOCAL_ALLOWANCE = ALLOWANCE
    ELSE                         ; LOCAL_ALLOWANCE = 5.0_REAL64
    END IF
    ! Initialize constants for algorithm.
    FIRST = 1
    LAST  = SIZE(VALUES)
    RANDOM_PIVOT = LOCAL_ALLOWANCE .GT. 0.0
    SEARCHED     = 0
    ! Loop until the index is found.
    DO WHILE (LAST - FIRST > 0)
       ! Compute the pivot value.
       IF (RANDOM_PIVOT) THEN ; PIVOT = (FIRST + LAST) / 2
       ELSE                   ; PIVOT = FIRST + MEDIAN_OF_MEDIANS(VALUES(FIRST:LAST)) - 1
       END IF
       PIVOT_VALUE = VALUES(PIVOT)
       ! Partition the data.
       I = PARTITION(VALUES(FIRST:LAST), PIVOT - FIRST + 1, INDEX - FIRST + 1)
       I = I + FIRST - 1
       ! Update the record of the amount of data searched.
       IF (RANDOM_PIVOT) SEARCHED = SEARCHED + (LAST - FIRST) + 1
       ! If the algorithm is done (partition found the element), then
       ! return, otherwise update 'FIRST' or 'LAST' depending on partition.
       IF ((I .EQ. INDEX) .AND. (VALUES(I) .EQ. PIVOT_VALUE)) THEN ; RETURN
       ELSE IF (I .LT. INDEX) THEN ; FIRST = I+1
       ELSE                        ; LAST  = I
       END IF
       ! If the amount searched has grown too large, switch to median-pivot.
       IF (RANDOM_PIVOT) RANDOM_PIVOT = (SEARCHED .LT. LOCAL_ALLOWANCE * SIZE(VALUES))
    END DO
  END SUBROUTINE INTROSELECT


  ! ------------------------------------------------------------------
  !                 Floyd-Rivest method for select
  ! ------------------------------------------------------------------

  ! left is the left index for the interval
  ! right is the right index for the interval
  ! k is the desired index value, where array[k] is the (k+1)th smallest element when left = 0
  RECURSIVE SUBROUTINE FLOYD_RIVEST(ARRAY, K, L, R)
    ! Arguments
    REAL(KIND=REAL64), INTENT(INOUT), DIMENSION(:) :: ARRAY
    INTEGER, INTENT(IN) :: K
    INTEGER, INTENT(IN), OPTIONAL :: L, R
    ! Locals
    INTEGER :: I, J, NEWLEFT, NEWRIGHT, LEFT, RIGHT
    REAL(KIND=REAL64) :: N, Z, S, SD, T
    ! Handle "L" optional.
    IF (PRESENT(L)) THEN ; LEFT = L
    ELSE                 ; LEFT = 1
    END IF
    ! Handle "R" optional.
    IF (PRESENT(R)) THEN ; RIGHT = R
    ELSE                 ; RIGHT = SIZE(ARRAY)
    END IF
    ! Loop until done finding the 'k'th element.
    DO WHILE (RIGHT .GT. LEFT)
       ! use select recursively to sample a smaller set of size s
       ! the arbitrary constants 600 and 0.5 are used in the original
       ! version to minimize execution time
       IF (RIGHT - LEFT .GT. 600) THEN
          N = RIGHT - LEFT + 1
          I = K - LEFT + 1
          Z = N
          Z = LOG(Z)
          S = 0.5 * EXP(2 * Z/3)
          SD = 0.5 * SQRT(Z * S * (N - S)/N)
          IF (I .LT. N/2) SD = -SD
          NEWLEFT = MAX(REAL(LEFT,REAL64), K - I * S/N + SD)
          NEWRIGHT = MIN(REAL(RIGHT,REAL64), K + (N - I) * S/N + SD)
          CALL FLOYD_RIVEST(ARRAY, K, NEWLEFT, NEWRIGHT)
       END if
       ! Pick a partition element from position "K".
       T = ARRAY(K)
       ! Initialize I and J to LEFT and RIGHT.
       I = LEFT
       J = RIGHT
       ! Move the partition element to the front of the list.
       CALL SWAP(ARRAY(LEFT), ARRAY(K))
       ! Pre-swap the left and right elements (temporarily putting a
       ! larger element on the left) before starting the partition loop.
       IF (ARRAY(RIGHT) .GT. T) CALL SWAP(ARRAY(LEFT), ARRAY(RIGHT))
       ! Now partition the elements about the pivot value "T".
       DO WHILE (I .LT. J)
          CALL SWAP(ARRAY(I), ARRAY(J))
          I = I + 1
          J = J - 1
          DO WHILE (ARRAY(I) .LT. T) ; I = I + 1 ; END DO
          DO WHILE (ARRAY(J) .GT. T) ; J = J - 1 ; END DO
       END DO
       ! Place the pivot element back into its appropriate place.
       IF (ARRAY(LEFT) .EQ. T) THEN
          CALL SWAP(ARRAY(LEFT), ARRAY(J))
       ELSE
          J = J + 1
          CALL SWAP(ARRAY(J), ARRAY(RIGHT))
       END IF
       ! adjust left and right towards the boundaries of the subset
       ! containing the (k - left + 1)th smallest element
       IF (J .LE. K) LEFT = J + 1
       IF (K .LE. J) RIGHT = J - 1
    END DO
  END SUBROUTINE FLOYD_RIVEST

  ! ------------------------------------------------------------------
  !                 Fast Select method (via TCH Lux)
  ! ------------------------------------------------------------------


  ! Given VALUES list of numbers, rearrange the elements of VALUES
  ! such that the element at index K has rank K (holds its same
  ! location as if all of VALUES were sorted).
  ! 
  ! This algorithm uses the same conceptual approach as Floyd-Rivest,
  ! but instead a standard-deviation based selection of bounds for
  ! recursion, a rank-based method is used to pick the subset of
  ! values that is searched. This simplifies the code and improves
  ! interpretability, while achieving the same tunable performance.
  ! 
  ! Arguments:
  ! 
  !   VALUES   --  A 1D array of real numbers.
  !   K        --  A positive integer for the rank index about which
  !                VALUES should be rearranged.
  ! Optional:
  ! 
  !   DIVISOR  --  A positive integer >= 2 that represents the
  !                division factor used for large VALUES arrays.
  !   MAX_SIZE --  An integer >= DIVISOR that represents the largest
  !                sized VALUES for which the worst-case pivot value
  !                selection is tolerable. A worst-case pivot causes
  !                O( SIZE(VALUES)^2 ) runtime. This value should be
  !                determined heuristically based on compute hardware.
  ! 
  ! Output:
  ! 
  !   The elements of the array VALUES are rearranged such that the
  !   element at position VALUES(K) is in the same location it would
  !   be if all of VALUES were in sorted order. Also known as,
  !   VALUES(K) has rank K.
  ! 
  RECURSIVE SUBROUTINE SELECT(VALUES, K, DIVISOR, MAX_SIZE)
    ! Arguments
    REAL(KIND=REAL64), INTENT(INOUT), DIMENSION(:) :: VALUES
    INTEGER, INTENT(IN) :: K
    INTEGER, INTENT(IN), OPTIONAL :: DIVISOR, MAX_SIZE
    ! Locals
    INTEGER :: LEFT, RIGHT, L, R, MS, D
    REAL(KIND=REAL64) :: P
    ! Initialize the divisor (for making subsets).
    IF (PRESENT(DIVISOR)) THEN ; D = DIVISOR
    ELSE IF (SIZE(VALUES) .GE. 2**23) THEN ; D = 2**5
    ELSE IF (SIZE(VALUES) .GE. 2**20) THEN ; D = 2**3
    ELSE                                   ; D = 2**2
    END IF
    ! Initialize the max size (before subsets are created).
    IF (PRESENT(MAX_SIZE)) THEN ; MS = MAX_SIZE
    ELSE                        ; MS = 2**10
    END IF
    ! Initialize LEFT and RIGHT to be the entire array.
    LEFT = 1
    RIGHT = SIZE(VALUES)
    ! Loop until done finding the 'k'th element.
    DO WHILE (LEFT .LT. RIGHT)
       ! Use SELECT recursively to improve the quality of the
       ! selected pivot value for large arrays.
       IF (RIGHT - LEFT .GT. MS) THEN
          ! Compute how many elements should be left and right of K
          ! to maintain the same percentile in a subset.
          L = K - K / D
          R = L + (SIZE(VALUES) / D)
          ! Perform fast select on an array half the size surrounding "k".
          CALL SELECT(VALUES(L:R), K - L + 1, DIVISOR, MAX_SIZE)
       END IF
       ! Pick a partition element at position "K".
       P = VALUES(K)
       L = LEFT
       R = RIGHT
       ! Move the partition element to the front of the list.
       CALL SWAP(VALUES(LEFT), VALUES(K))
       ! Pre-swap the left and right elements (temporarily putting a
       ! larger element on the left) before starting the partition loop.
       IF (VALUES(RIGHT) .GT. P) CALL SWAP(VALUES(LEFT), VALUES(RIGHT))
       ! Now partition the elements about the pivot value "T".
       DO WHILE (L .LT. R)
          CALL SWAP(VALUES(L), VALUES(R))
          L = L + 1
          R = R - 1
          DO WHILE (VALUES(L) .LT. P) ; L = L + 1 ; END DO
          DO WHILE (VALUES(R) .GT. P) ; R = R - 1 ; END DO
       END DO
       ! Place the pivot element back into its appropriate place.
       IF (VALUES(LEFT) .EQ. P) THEN
          CALL SWAP(VALUES(LEFT), VALUES(R))
       ELSE
          R = R + 1
          CALL SWAP(VALUES(R), VALUES(RIGHT))
       END IF
       ! adjust left and right towards the boundaries of the subset
       ! containing the (k - left + 1)th smallest element
       IF (R .LE. K) LEFT = R + 1
       IF (K .LE. R) RIGHT = R - 1
    END DO
  END SUBROUTINE SELECT


END MODULE FAST_SELECT

