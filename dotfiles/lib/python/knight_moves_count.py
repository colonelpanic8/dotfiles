#!/usr/bin/env python
import sys


class KnightMoves(object):

    deltas = [(1, 2), (2, 1), (-1, 2), (-2, 1),
              (1, -2), (2, -1), (-1, -2), (-2, -1)]

    max_x = 8
    max_y = 8

    def count_knight_moves(self, start, end):
        this_generation = [start]
        move_count = 0
        seen = set()

        while True:
            for position in this_generation:
                if position in seen:
                    continue
                elif position == end:
                    return move_count
                else:
                    seen.add(position)

            this_generation = list(self.generate_moves_from_generation(this_generation))
            move_count += 1

    def generate_moves_from_generation(self, previous_generation):
        return (
            position
            for ancestor in previous_generation
            for position in self.generate_moves_from_position(ancestor)
        )

    def generate_moves_from_position(self, position):
        x, y = position
        return (
            (x + delta_x, y + delta_y)
            for delta_x, delta_y in self.deltas
            if self.in_bounds(x + delta_x, y + delta_y)
        )

    def in_bounds(self, x, y):
        return 0 <= x < self.max_x and 0 <= y < self.max_y


def file_to_index(file_char):
    assert 'a' <= file_char <= 'h'
    return ord(file_char) - 97


def rank_to_index(rank):
    assert 0 < int(rank) <= 8
    return int(rank) - 1


def square_name_to_indices(square_name):
    file_char, rank_char = square_name
    return rank_to_index(int(rank_char)), file_to_index(file_char)


if __name__ == '__main__':
    print KnightMoves().count_knight_moves(
        square_name_to_indices(sys.argv[1]),
        square_name_to_indices(sys.argv[2])
    )
