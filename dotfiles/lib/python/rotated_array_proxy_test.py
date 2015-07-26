import rotated_array

# duplicates, slicing with stride, insertion index for item greater than everything for completely sorted array
def test_empty_rotated_array_proxy():
    empty_rap = rotated_array.RotatedArrayProxy([])
    assert empty_rap.rotation_index == 0
    assert empty_rap.unrotated() == []
    assert empty_rap.sorted_insertion_index(100) == 0
    assert empty_rap.actual_insertion_index(100) == 0


def test_inserting_at_end_of_insertion_range():
    rap = rotated_array.RotatedArrayProxy([3, 4, 5, 0, 2])
    assert rap.rotation_index == 3
    assert rap.unrotated() == [0, 2, 3, 4, 5]

    assert rap.sorted_insertion_index(-1) == 0
    assert rap.actual_insertion_index(-1) == 3

    assert rap.sorted_insertion_index(1) == 1
    assert rap.actual_insertion_index(1) == 4

    assert rap.sorted_insertion_index(2) in [1, 2]
    assert rap.actual_insertion_index(2) in [4, 5]

    assert rap.sorted_insertion_index(3) in [2, 3]
    assert rap.actual_insertion_index(3) in [0, 1]


def test_inserting_for_sorted_array():
    rap = rotated_array.RotatedArrayProxy([0, 1])
    assert rap.unrotated() == [0, 1]
    assert rap.sorted_insertion_index(1000) == 2
    assert rap.actual_insertion_index(1000) == 2


def test_inserting_largest_element():
    rap = rotated_array.RotatedArrayProxy([3, 0, 1])
    assert rap.rotation_index == 1
    assert rap.sorted_insertion_index(1000) == 3
    assert rap.actual_insertion_index(1000) == 1


def test_inserting_largest_element():
    rap = rotated_array.RotatedArrayProxy([3, 0, 1])
    assert rap.unrotated() == [0, 1, 3]
    assert rap.actual_insertion_index(2) == 0
    assert rap.actual_insertion_index(1) in [2, 3]


def test_rotation_index_and_unrotate():
    arr = [3]*117 + [1] + [2]*16
    rap = rotated_array.RotatedArrayProxy(arr)
    assert rap[0] == 1
    assert rap.rotation_index == 117
    assert rap.unrotated() == sorted(arr)

    arr = [3, 3, 3, 3, 1, 1, 1, 2, 2]
    rap = rotated_array.RotatedArrayProxy(arr)
    assert rap.rotation_index == 4
    assert rap.unrotated() == sorted(arr)

    rap = rotated_array.RotatedArrayProxy([3, 3, 3, 3, 1, 1, 1, 2])
    assert rap.rotation_index == 4
    assert rap.unrotated() == [1, 1, 1, 2, 3, 3, 3, 3]


def test_insert():
    arr = [3]*117 + [1] + [2]*16
    rap = rotated_array.RotatedArrayProxy(arr)
    rap.insert(3)
    rap.insert(3)
    rap.insert(2)
    rap.insert(2)
    rap.insert(5)
    rap.insert(24)
    rap.insert(5)
    rap.insert(4)
    rap.insert(4)
    assert rap.unrotated() == sorted(rap.incoming)
