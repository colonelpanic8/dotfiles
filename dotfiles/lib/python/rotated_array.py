def rotate_array(incoming, rotation_index):
    new_back = incoming[:rotation_index]
    new_front = incoming[rotation_index:]
    new_front.extend(new_back)
    return new_front


def binary_search(
    array, item, low=0, high=None,
    lower_predicate=lambda item, array, index, low, high: item <= array[index]
):
    if low < 0:
        raise ValueError('lo must be non-negative')
    if high is None:
        high = len(array)
    while low < high:
        mid = (low + high)//2
        if lower_predicate(item, array, mid, low, high):
            high = mid
        else:
            low = mid + 1
    return low


class RotatedArrayProxy(object):

    def __init__(self, incoming):
        self.incoming = incoming
        self._rotation_index = None
        if incoming:
            # Duplicates can not span the rotation
            assert incoming[0] != incoming[-1]

    def __getitem__(self, item):
        if not isinstance(item, slice):
            return self.incoming[self._actual_index(item)]
        else:
            self._handle_slice(item)

    def _actual_index(self, index):
        if index is None:
            return index
        elif 0 <= index < len(self.incoming):
            return (index + self.rotation_index) % len(self.incoming)
        elif index == len(self.incoming):
            return self.rotation_index
        else:
            raise Exception()

    @property
    def rotation_index(self):
        if self._rotation_index is None:
            self._rotation_index = self._find_rotation_index()
        return self._rotation_index

    def _find_lower_predicate(self, item, array, index, low, high):
        return array[0] > array[index]

    def _find_rotation_index(self):
        if len(self.incoming) < 1:
            return 0
        return binary_search(self.incoming, self.incoming[0],
                             lower_predicate=self._find_lower_predicate)

    def __len__(self):
        return len(self.incoming)

    def sorted_insertion_index(self, x):
        return binary_search(self, x)

    def actual_insertion_index(self, x):
        return self._actual_index(self.sorted_insertion_index(x))

    def unrotated(self):
        return rotate_array(self.incoming, self.rotation_index)

    def insert(self, x):
        insertion_index = self.actual_insertion_index(x)
        if insertion_index < self.rotation_index or (
            insertion_index == self.rotation_index and
            (not self.incoming or x < self.incoming[0])
        ):
            self._rotation_index += 1
        self.incoming.insert(self.actual_insertion_index(x), x)
