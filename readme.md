|             |                |
|-------------|----------------|
|**TITLE:**   | quickselect    |
|**PURPOSE:** | A Fortran implementation of quickselect. |
|**AUTHOR:**  | Thomas C.H. Lux  |
|**EMAIL:**   | tchlux@vt.edu |


## INSTALLATION:

    git clone https://github.com/tchlux/quickselect.git
    cd quickselect
    pip3 install --user fmodpy
    python3 test.py


## PYTHON USAGE:

    import fmodpy
    qs = fmodpy.fimport("quickselect.f90")
    
    import numpy as np
    array = np.random.random(size=(100,))
    rank = 50
    
    qs.select( array, rank )
    print(f"Rank {rank} element:", array[rank])
    
    # ^^ Notice the array has been partially sorted in-place 
    #    about the rank-50 element.


## HOW IT WORKS:

  The `fmodpy` module automatically constructs the python interface to
  the Fortran quickselect code. The code itself initially uses the
  middle element of the array as a pivot and partitions, recursively
  searching the remaining half of the array that will contain the
  desired rank. If the convergence of this technique is too slow, the
  pivot value is chosen by computing the median-of-medians (a value in
  the array guaranteed to be between the 30th and 70th percentiles).

  The median-of-medians computation is `O(log(n))`, however at each
  step n will be shrinking by at least 30%, making the pivot
  computation negligible. The dominating runtime will be the repeated
  division of the array in half, with a final complexity of about
  `O(n)`. In truth, the complexity is slightly larger, but only a
  small amount that will not affect most applications.

  The steps of the algorithm look like:

    - pick pivot value randomly
    - partition array by pivot
    - search the elements on the rank `k` side of the pivot
    - if the size of the search space is not shrinking fast enough,
      select pivot values with median-of-medians algorithm.