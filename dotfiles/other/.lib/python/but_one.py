def left_partials(incoming):
    product = 1
    for i in incoming:
        product *= i
        yield product


def but_one(incoming):
    """Given an array `incoming` return an array whose ith index is the
    sum of all the elements of `incoming` except for `incoming[i]`
    """
    lpartials = list(left_partials(incoming))
    rproduct = 1
    result = [None] * len(incoming)
    for i in range(len(incoming)):
        back_index = len(incoming) - i - 1
        if back_index > 0:
            result[back_index] = rproduct * lpartials[back_index-1]
            if back_index < len(incoming):
                rproduct *= incoming[back_index]
        else:
            result[back_index] = rproduct
    return result
