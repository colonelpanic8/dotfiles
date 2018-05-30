import operator


def generate_decreasing_n_sequence_with_bounded_sum(
    sequence_length, sum_bound, value_bound=float('inf'),
):
    if sequence_length == 0:
        yield []
        return
    min_remaining = sequence_length*(sequence_length - 1)/2
    bound_for_current = min(sum_bound - min_remaining, value_bound)

    for value in range(sequence_length, bound_for_current):
        for sequence in generate_decreasing_n_sequence_with_bounded_sum(
            sequence_length - 1, sum_bound - value, value_bound=value,
        ):
            yield [value] + sequence


def build_products_to_sequences_map():
    product_to_sequences_map = {}
    for sequence in generate_decreasing_n_sequence_with_bounded_sum(4, 18):
        product = reduce(operator.mul, sequence, 1)
        product_to_sequences_map.setdefault(product, []).append(sequence)
    return product_to_sequences_map
