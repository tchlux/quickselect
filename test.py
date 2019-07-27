import numpy as np

import fmodpy
fs = fmodpy.fimport("fast_select.f90", autocompile_extra_files=True)

# Test function for "bubble_median" function.
def _test_bubble_sort():
    for i in range(1000):
        for j in range(1,10):
            v = np.random.random(size=(j,))
            fs.bubble_sort(v)
            try: assert( sum(v - np.array(sorted(v))) == 0 )
            except: print("ERROR:",v)
 
# Test the function "partition".
def _test_partition():
    # Case 1
    v = np.array([3, 1, 1, 3, 4, 5], dtype=float)
    out = fs.partition(v, 6, 2)
    assert(out == 5)
    assert(sum(v - np.array([3,1,1,3,4,5], dtype=float)) == 0)
    # Case 2
    v = np.array([3, 1, 1, 3, 4, 5], dtype=float)
    out = fs.partition(v, 1, 2)
    assert(out == 2)
    assert(sum(v - np.array([1,1,5,3,4,3], dtype=float)) == 0)
    # Case 3
    v = np.array([3, 1, 1, 3, 4, 5], dtype=float)
    out = fs.partition(v, 1, 5)
    assert(out == 4)
    assert(sum(v - np.array([1,1,3,3,4,5], dtype=float)) == 0)
    # Case 4
    v = np.array([3, 4, 1, 3, 1, 5], dtype=float)
    out = fs.partition(v, 2, 1)
    assert(out == 4)
    assert(sum(v - np.array([3,1,3,1,4,5], dtype=float)) == 0)
    # Case 5 (partition entire array by an element, provide no INDEX)
    v = np.array([3, 4, 1, 3, 5, 4], dtype=float)
    out = fs.partition(v, 1)
    assert(out == 3)
    assert(sum(v - np.array([1,3,3,4,5,4], dtype=float)) == 0)
    # Case 6
    v = np.array([1, 4, 3, 3, 1, 5], dtype=float)
    out = fs.partition(v, 1, 3)
    assert(out == 2)
    assert(sum(v - np.array([1,1,3,3,5,4], dtype=float)) == 0)
    # Case 7
    np.random.seed(0)
    v = np.random.random(size=101)
    fs.partition(v, 1+np.argmin(abs(v - np.median(v))))
    assert( np.all(v[:len(v)//2] <= np.median(v)) )
    assert( np.all(v[len(v)//2:] >= np.median(v)) )


def _test_select_func(select):
    # Case 1
    v = np.array([1, 4, 3, 1, 3, 5], dtype=float)
    i = 1
    select(v, i)
    assert(v[i-1] == 1)
    # Case 2
    v = np.array([1, 4, 3, 3, 1, 5], dtype=float)
    i = 2
    select(v, i)
    assert(v[i-1] == 1)
    # Case 3
    v = np.array([1, 4, 5, 3, 1, 3], dtype=float)
    i = 3
    select(v, i)
    assert(v[i-1] == 3)
    # Case 4
    v = np.array([1, 4, 5, 3, 1, 3], dtype=float)
    i = 5
    select(v, i)
    assert(v[i-1] == 4)
    # Case 5
    for i in range(1, 7):
        true_v = np.array([1, 1, 3, 3, 4, 5], dtype=float)
        v = np.array([1, 1, 3, 3, 4, 5], dtype=float)
        select(v, i)
        assert(v[i-1] == true_v[i-1])
    # Case 6
    for seed in range(1000):
        np.random.seed(seed)
        v = np.random.random(size=13)
        select(v, len(v)//2 + 1)
        try: assert(v[len(v)//2] == np.median(v))
        except:
            np.random.seed(seed)
            v = np.random.random(size=13)
            print()
            print("ERROR")
            print()
            print(v)
            select(v, len(v)//2 + 1, debug=True)
            print(v)
            print()
            print("median", np.median(v))
            print("value ", v[len(v)//2])
            print()
            exit()
            
        assert( np.all( v[:len(v)//2] <= v[len(v)//2]) )



# Timed test code for comparison.
# 
#   size   -- positive integer
#   mode   -- "random" or "sorted"
#   select -- "median" or "min"
# 
def _test_timing(size=10000, mode="random", select="median"):
    print()
    print(f"{size} values")
    print(f"      mode -- {mode}")
    print(f" selecting -- {select}")
    # 
    # Testing re-use function.
    def test(func, name):
        from util.system import Timer
        t = Timer()
        # Initialize the data
        if mode == "random":
            np.random.seed(0)
            v = np.random.random(size=size)
        elif mode == "sorted":
            v = np.ones(size) * np.arange(size)
        # Time the algorithm
        t.start()
        if select == "median":
            func(v, len(v)//2)
        elif select == "min":
            func(v, 1)
        t.stop()
        print(f"{name} time:", t())
        del(v)

    # Run tests on different algorithms.
    print()
    test(fs.introselect,  "IS")
    test(fs.floyd_rivest, "FR")
    test(fs.select,       " S")
    test(np.partition,    "NP")



print("testing bubble_sort..")
_test_bubble_sort()
print("testing partition..")
_test_partition()
print("testing introselect..")
_test_select_func(fs.introselect)
print("testing floyd_rivest..")
_test_select_func(fs.floyd_rivest)
print("testing select..")
_test_select_func(fs.select)
print("testing timing..")
_test_timing()


