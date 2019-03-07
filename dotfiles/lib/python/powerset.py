import collections
import copy


def powerset(elems):
    counts = collections.defaultdict(int)
    for elem in elems:
        counts[elem] += 1
    return powerset_helper(counts.items())


def powerset_helper(elems):
    last_generation = [[]]
    for (elem, count) in elems:
        next_generation = last_generation
        for _ in range(count):
            new_generation = []
            for subset in last_generation:
                new_subset = copy.copy(subset)
                new_subset.append(elem)
                new_generation.append(new_subset)
            next_generation.extend(new_generation)
            last_generation = new_generation
        last_generation = next_generation
    return last_generation


if __name__ == '__main__':
    print(len(powerset(range(23))))
