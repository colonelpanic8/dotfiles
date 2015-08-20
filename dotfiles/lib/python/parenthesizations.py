from backports.functools_lru_cache import lru_cache


def parenthesizations(pairs):
    parenthesizations = _parenthesizations(pairs * 2, net=0)
    return call_count, parenthesizations


@lru_cache(maxsize=None)
def _parenthesizations(length, net=0):
    if net > length or net < 0:
        raise Exception()
    if net == length:
        return [')' * length]
    res = prepend('(', _parenthesizations(length-1, net=net + 1))
    if net > 0:
        res.extend(prepend(')', _parenthesizations(length-1, net=net - 1)))
    return res


def prepend(char, items):
    return [char + item for item in items]
