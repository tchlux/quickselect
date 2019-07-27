import numpy as np

import fmodpy
qs = fmodpy.fimport("quickselect.f90", autocompile_extra_files=True)

# Test function for "bubble_median" function.
def _test_bubble_sort():
    for i in range(1000):
        for j in range(1,10):
            v = np.random.random(size=(j,))
            qs.bubble_sort(v)
            try: assert( sum(v - np.array(sorted(v))) == 0 )
            except: print("ERROR:",v)
 
# Test the function "partition".
def _test_partition():
    # Case 1
    v = np.array([3, 1, 1, 3, 4, 5], dtype=float)
    out = qs.partition(v, 6, 2)
    assert(out == 5)
    assert(sum(v - np.array([3,1,1,3,4,5], dtype=float)) == 0)
    # Case 2
    v = np.array([3, 1, 1, 3, 4, 5], dtype=float)
    out = qs.partition(v, 1, 2)
    assert(out == 2)
    assert(sum(v - np.array([1,1,5,3,4,3], dtype=float)) == 0)
    # Case 3
    v = np.array([3, 1, 1, 3, 4, 5], dtype=float)
    out = qs.partition(v, 1, 5)
    assert(out == 4)
    assert(sum(v - np.array([1,1,3,3,4,5], dtype=float)) == 0)
    # Case 4
    v = np.array([3, 4, 1, 3, 1, 5], dtype=float)
    out = qs.partition(v, 2, 1)
    assert(out == 4)
    assert(sum(v - np.array([3,1,3,1,4,5], dtype=float)) == 0)
    # Case 5 (partition entire array by an element, provide no INDEX)
    v = np.array([3, 4, 1, 3, 5, 4], dtype=float)
    out = qs.partition(v, 1)
    assert(out == 3)
    assert(sum(v - np.array([1,3,3,4,5,4], dtype=float)) == 0)

    # Case 3
    v = np.array([1, 4, 3, 3, 1, 5], dtype=float)
    out = qs.partition(v, 1, 3)
    assert(out == 2)
    assert(sum(v - np.array([1,1,3,3,5,4], dtype=float)) == 0)


def _test_selectr():
    # Case 1
    v = np.array([1, 4, 3, 1, 3, 5], dtype=float)
    i = 1
    qs.selectr(v, i)
    assert(v[i-1] == 1)
    # Case 2
    v = np.array([1, 4, 3, 3, 1, 5], dtype=float)
    i = 2
    qs.selectr(v, i)
    assert(v[i-1] == 1)
    # Case 3
    v = np.array([1, 4, 5, 3, 1, 3], dtype=float)
    i = 3
    qs.selectr(v, i)
    assert(v[i-1] == 3)
    # Case 4
    v = np.array([1, 4, 5, 3, 1, 3], dtype=float)
    i = 5
    qs.selectr(v, i)
    assert(v[i-1] == 4)
    # Case 5
    for i in range(1, 7):
        true_v = np.array([1, 1, 3, 3, 4, 5], dtype=float)
        v = np.array([1, 1, 3, 3, 4, 5], dtype=float)
        qs.selectr(v, i)
        assert(v[i-1] == true_v[i-1])


def _test_select():
    # Case 1
    v = np.array([1, 4, 3, 1, 3, 5], dtype=float)
    i = 1
    qs.select(v, i)
    assert(v[i-1] == 1)
    # Case 2
    v = np.array([1, 4, 3, 3, 1, 5], dtype=float)
    i = 2
    qs.select(v, i)
    assert(v[i-1] == 1)
    # Case 3
    v = np.array([1, 4, 5, 3, 1, 3], dtype=float)
    i = 3
    qs.select(v, i)
    assert(v[i-1] == 3)
    # Case 4
    v = np.array([1, 4, 5, 3, 1, 3], dtype=float)
    i = 5
    qs.select(v, i)
    assert(v[i-1] == 4)
    # Case 5
    for i in range(1, 7):
        true_v = np.array([1, 1, 3, 3, 4, 5], dtype=float)
        v = np.array([1, 1, 3, 3, 4, 5], dtype=float)
        qs.select(v, i)
        assert(v[i-1] == true_v[i-1])

_test_bubble_sort()
_test_partition()
_test_selectr()
_test_select()

from util.system import Timer
t = Timer()
size = 1000
print("Finding median..")

print()
np.random.seed(0)
v = np.random.random(size=size)
t.start()
qs.select(v, len(v)//2)
t.stop()
print(v[:10])
print("QS Time:", t())


print()
np.random.seed(0)
v = np.random.random(size=size)
t.start()
v.partition(len(v)//2)
t.stop()
print(v[:10])
print("NP Time:", t())

