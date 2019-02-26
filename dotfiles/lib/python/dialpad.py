# neighbors = {
#     1: [6, 8],
#     2: [7, 9],
#     3: [8, 4],
#     4: [9, 3, 0],
#     5: [],
#     6: [0, 7, 1],
#     7: [2, 6],
#     8: [1, 3],
#     9: [4, 2],
#     0: [4, 6]
# }

# cache = {}

# def count_numbers(current_number, number_of_hops):
#     cache_value = (current_number, number_of_hops)
#     if cache_value in cache:
#         return cache[cache_value]

#     if number_of_hops == 1:
#         return 1
#     number_count = 0
#     for neighbor in neighbors[current_number]:
#         number_count += count_numbers(neighbor, number_of_hops - 1)

#     cache[cache_value] = number_count
#     return number_count


class DialpadCounter(object):

    knight_deltas = [
        (2, 1),
        (-2, -1),
        (-2, 1),
        (2, -1),
        (1, 2),
        (-1, -2),
        (-1, 2),
        (1, -2)
    ]

    def __init__(self, dialpad_matrix):
        self._matrix = dialpad_matrix
        self._row_size = len(dialpad_matrix[0])
        self._row_count = len(dialpad_matrix)
        self._cache = {}

    def neighbors(self, y, x):
        result = []
        for delta_y, delta_x in self.knight_deltas:
            neighbor_y = delta_y + y
            neighbor_x = delta_x + x
            neighbor = (neighbor_y, neighbor_x)
            if (self.inbounds(neighbor_y, neighbor_x) and
                self._matrix[neighbor_y][neighbor_x]):
                result.append(neighbor)
        return result

    def inbounds(self, y, x):
        return 0 <= x < self._row_size and 0 <= y < self._row_count

    def count_numbers(self, coordinate, number_of_hops):
        y, x = coordinate
        if not self._matrix[y][x]:
            raise Exception()

        cache_value = (coordinate, number_of_hops)

        if cache_value in self._cache:
            return self._cache[cache_value]

        if number_of_hops == 1:
            return 1

        number_count = 0
        for neighbor in self.neighbors(y, x):
            number_count += self.count_numbers(neighbor, number_of_hops - 1)

        self._cache[cache_value] = number_count

        return number_count

def count_numbers(number, number_of_hops):
    matrix = [
        [True, True, True],
        [True, True, True],
        [True, True, True],
        [False, True, False]
    ]
    if number == 0:
        coordinate = 3, 1
    else:
        row = (number - 1) // 3
        column = (number - 1) % 3
        coordinate = (row, column)
    counter = DialpadCounter(matrix)
    return counter.count_numbers(coordinate, number_of_hops)

if __name__ == '__main__':
    print(count_numbers(1, 1))
    print(count_numbers(1, 2))
    print(count_numbers(1, 3))
    print(count_numbers(1, 4))
    print(count_numbers(1, 10))
    print(count_numbers(1, 30))
