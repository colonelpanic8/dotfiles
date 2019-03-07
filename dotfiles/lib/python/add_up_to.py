def count_unique_sums2(number, maximum_size):
    maximum_to_try = min(number, maximum_size) + 1
    the_range = list(range(1, maximum_to_try))

    all_sums = []
    for max_in_sum in the_range:
        if max_in_sum == number:
            all_sums.extend([[number]])
            continue
        new_sums = count_unique_sums(number-max_in_sum, max_in_sum)
        all_sums.extend(
            [
                sum_so_far + [max_in_sum]
                for sum_so_far in new_sums
            ]
        )

    return all_sums


unique_sum_counts = {}


def count_unique_sums(number, maximum_size):
    if (number, maximum_size) in unique_sum_counts:
        return unique_sum_counts[(number, maximum_size)]

    maximum_to_try = min(number, maximum_size) + 1
    the_range = list(range(1, maximum_to_try))

    sum_count = 0
    for max_in_sum in the_range:
        if max_in_sum == number:
            sum_count += 1
            continue
        sum_count += count_unique_sums(number-max_in_sum, max_in_sum)

    unique_sum_counts[(number, maximum_size)] = sum_count
    return sum_count


if __name__ == '__main__':
    print(count_unique_sums(100, 100))
