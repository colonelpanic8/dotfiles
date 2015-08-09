class Result(object):

    def __init__(self, lb, excluded, rb, max):
        self.lb = lb
        self.excluded = excluded
        self.rb = rb
        self.max = max

    def __repr__(self):
        return "left bound: {0}, excluded {1}, right_bound {2}, max {3}".format(
            self.lb,
            self.excluded,
            self.rb,
            self.max,
        )



def max_double_slice(array):
    left_slices = max_slice_at_index(array)
    right_slices = max_slice_at_index(array[::-1])[::-1]
    print left_slices
    print right_slices
    def slice_sum(index):
        left_contribution = left_slices[index - 1][-1] if index > 0 else 0
        right_contribution = right_slices[index + 1][-1] if index < len(array) - 2 else 0
        return right_contribution + left_contribution
    maximizing_slice_index = max((i for i in range(len(array))), key=slice_sum)
    left_bound, lmax = left_slices[maximizing_slice_index - 1] if maximizing_slice_index > 0 else (0, 0)
    rs, rmax = right_slices[maximizing_slice_index + 1] if maximizing_slice_index < len(array) - 2 else (len(array) - 1, 0)
    right_bound = len(array) - 1 - rs
    return Result(left_bound, maximizing_slice_index, right_bound, lmax + rmax)


def max_double_slice_value(array):
    left, right = max_double_slice(array)
    return left[-1] + right[-1]


def max_slice_at_index(array):
    max_at_index = []
    for index, array_value in enumerate(array):
        last_start, last_max = max_at_index[-1] if max_at_index else (index, 0)
        if last_max < 0:
            current_max = array_value
            current_start = index
        else:
            current_max = array_value + last_max
            current_start = last_start
        max_at_index.append((current_start, current_max))
    return max_at_index
