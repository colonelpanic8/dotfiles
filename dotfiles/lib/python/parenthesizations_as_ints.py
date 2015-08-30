from backports.functools_lru_cache import lru_cache


def parenthesizations2h(pair_count):
    return [
        '{0:b}'.format(item).replace('1', '(').replace('0', ')')
        for item in parenthesizations.parenthesizations(pair_count)
    ]

def parenthesizations2(pair_count):
    """Parenthesizations returned encoded as numbers
    """
    parenthesizations = _parenthesizations2(pair_count * 2, net=0)
    return parenthesizations


@lru_cache(maxsize=None)
def _parenthesizations2(length, net=0):
    if net > length or net < 0:
        raise Exception()
    if net == length:
        return [0]
    res = add_bit(length-1, _parenthesizations(length-1, net=net + 1))
    if net > 0:
        res.extend(_parenthesizations(length-1, net=net - 1))
    return res


def add_bit(bitindex, items):
    value = 2 ** bitindex
    return [value + item for item in items]
