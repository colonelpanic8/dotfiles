def groupings(numbers):
    if len(numbers) == 0:
        raise StopIteration()
    if len(numbers) == 1:
        yield ((numbers[0],),)
        raise StopIteration()
    this_number = numbers[0]
    this_tuple = (this_number,)
    next_groupings = groupings(numbers[1:])
    for grouping in next_groupings:
        yield (this_tuple,) + grouping
        yield (this_tuple + grouping[0],) + grouping[1:]


def plus_minus_n(numbers, n):
    for grouping in groupings(numbers):
        numbers = map(group_to_int, grouping)
        pms = plus_minuses(len(numbers) - 1)
        for pm in pms:
            result = compute(numbers, pm)
            string = generate_string(numbers, pm)
            if result == n:
                yield string


def generate_string(numbers, pms):
    string = ''
    for number in numbers:
        string += '{0}'.format(number)
        if pms:
            string += '+' if pms[0] is PLUS else '-'
            pms = pms[1:]
    return string


def compute(numbers, plus_minus):
    sum = numbers[0]
    remaining = numbers[1:]
    while remaining:
        if plus_minus[0] == PLUS:
            sum += remaining[0]
        else:
            sum -= remaining[0]
        remaining = remaining[1:]
        plus_minus = plus_minus[1:]
    return sum


def group_to_int(group):
    return int(''.join(map(str, group)))


PLUS = object()
MINUS = object()


def plus_minuses(n):
    if n == 0:
        yield ()
        raise StopIteration()
    for pm in plus_minuses(n-1):
        yield (PLUS,) + pm
        yield (MINUS,) + pm
