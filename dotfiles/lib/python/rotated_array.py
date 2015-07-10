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

    def _handle_slice(self, item):
        start = self._actual_index(item.start)
        stop = self._actual_index(item.stop)
        if start > stop:
            sliced = self.incoming[start::item.stride]
            # Ensure that the stride is preserved as it passes through
            # the rotation.
            start_index = (len(self.incoming) - start) % (item.stride or 1)
            if start_index <= stop:
                sliced.extend(
                    self.incoming[start_index:stop:item.stride]
                )
            return sliced
        else:
            return self.incoming[start:stop:item.stride]

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
