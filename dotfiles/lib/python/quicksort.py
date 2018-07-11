def quicksort(incoming, lower=0, upper=None):
    if upper is None:
        upper = len(incoming)
    if upper - lower < 2:
        return
    low_swap = lower
    high_swap = upper - 1
    replacing = high_swap
    pivot = incoming[high_swap]
    high_swap -= 1
    while True:
        if replacing > low_swap:
            candidate = incoming[low_swap]
            if candidate > pivot:
                incoming[replacing] = candidate
                replacing = low_swap
            if low_swap == high_swap:
                break
            low_swap += 1
        else:
            candidate = incoming[high_swap]
            if candidate < pivot:
                incoming[replacing] = candidate
                replacing = high_swap
            if low_swap == high_swap:
                break
            high_swap -= 1
    incoming[replacing] = pivot
    quicksort(incoming, lower=lower, upper=replacing)
    quicksort(incoming, lower=replacing+1, upper=upper)


if __name__ == '__main__':
    my_list = [3, 20, 2, 52, 44, 16, 24, 5, 12, 4, 1, 14, 60, 29, 33, 1]
    quicksort(my_list)
    print(my_list)
