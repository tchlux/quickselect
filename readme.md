|             |                |
|-------------|----------------|
|**TITLE:**   | Fast Select    |
|**PURPOSE:** | A Fortran implementation of multiple `select` algorithms. |
|**AUTHOR:**  | Thomas C.H. Lux  |
|**EMAIL:**   | tchlux@vt.edu |


## INSTALLATION:

    git clone https://github.com/tchlux/quickselect.git
    cd quickselect
    pip3 install --user fmodpy
    python3 test.py


## PYTHON USAGE:

    import fmodpy
    fs = fmodpy.fimport("fast_select.f90")
    
    import numpy as np
    array = np.random.random(size=(100,))
    rank = 50
    
    fs.select( array, rank )
    print(f"Rank {rank} element:", array[rank])
    
    # ^^ Notice the array has been partially sorted in-place 
    #    about the rank-50 element.


## ABOUT:

  The `fmodpy` module automatically constructs the python interface to
  the Fortran fast select module. The code itself implements three
  different strategies for performing the `select` operation (called
  `partition` in NumPy). The three algorithms implemented are:

   * [Introselect](https://en.wikipedia.org/wiki/Introselect) -- a
     selection method that starts off with [quickselect](https://en.wikipedia.org/wiki/Quickselect)
     with a random pivot selection and transitions to [median of medians](https://en.wikipedia.org/wiki/Median_of_medians)
     when poor convergence is observed to guarantee `O(n)` performance.

   * [Floyd-Rivest](https://en.wikipedia.org/wiki/Floyd–Rivest_algorithm)
     selection, which also exhibits `O(n)` runtime, but has a smaller
     constant in practice than introselect (heuristically fewer
     comparisons needed).

   * A novel method called *Fast Select* based on the F-R method, but
     with a simpler recursion condition and slightly faster
     performance for some problems.

  Each of these techniques has good worst-case performance, but only
  F-R and FastSelect compete with the speed of `ndarray.partition`
  from the NumPy module for Python. For randomly ordered and fully
  sorted lists of most sizes (ranging from 100 to 1B elements) the F-R
  and FastSelect methods are faster than NumPy (by up to 100%).
