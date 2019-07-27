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


_test_bubble_sort()
_test_partition()
_test_selectr()





# This function rearranges a set of provided points into a vantage
# point tree and returns the split radius and shell size of all nodes
# in the tree on output.
# 
# The tree is stored implicitly with each 
# 
# Arguments:
#   points    -- matrix of floats (D x N)
#   leaf_size -- positive integer determining points per leaf node.
#   random    -- method for determining center, True -> random selection,
#                False -> point furthest from center of mass.
# 
# Returns:
#   points -- matrix of floats (D x N)
#   splits -- vector of floats for non-leaf node split radii (N)
#   shells -- vector of floats for node outer shell distance (N)
def build_vp_tree(points, leaf_size=10, random=True):
    
    if not random:
        # Compute the center of mass and make it the first point.
        pass
    return points, splits, shells


 
   

