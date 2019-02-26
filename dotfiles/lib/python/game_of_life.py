#! /usr/bin/env python

class GameOfLife(object):

    neighbor_deltas = [
        (1, 0), (1, 1), (1, -1),
        (0, 1), (0, -1),
        (-1, 0), (-1, 1), (-1, -1)
    ]

    @classmethod
    def empty_with_size(cls, rows, columns=None):
        columns = columns or rows
        return cls([])

    @staticmethod
    def build_empty_grid(rows, columns):
        return [[False for _ in range(columns)] for _ in range(rows)]

    def __init__(self, initial_state):
        self.current_state = initial_state
        self.row_count = len(initial_state)
        self.column_count = len(initial_state[0])

    def _neighbors(self, row, column):
        for (row_delta, column_delta) in self.neighbor_deltas:
            candidate_row = row + row_delta
            candidate_column = column + column_delta
            if self._in_bounds(candidate_row, candidate_column):
                yield candidate_row, candidate_column

    def _in_bounds(self, row, column):
        return 0 <= row < self.row_count and 0 <= column < self.column_count

    def _next_state_for_cell(self, row, column):
        live_count = 0
        cell_was_live = self.current_state[row][column]
        for neighbor_row, neighbor_column in self._neighbors(row, column):
            if self.current_state[neighbor_row][neighbor_column]:
                live_count += 1
        if cell_was_live:
            return 1 < live_count < 4
        else:
            return live_count == 3

    def compute_next_game_state(self, new_state=None):
        new_state = new_state or self.build_empty_grid(
            self.row_count, self.column_count,
        )
        for row in range(self.row_count):
            for column in range(self.column_count):
                new_state[row][column] = self._next_state_for_cell(row, column)
        return new_state

    def tick(self, new_state=None):
        self.current_state = self.compute_next_game_state(new_state)

    def _build_row_string(self, row):
        return " ".join(["o" if state else "." for state in row])

    @property
    def state_string(self):
        return "\n".join(
            self._build_row_string(row) for row in self.current_state
        )

    @classmethod
    def run(cls, initial_state, generations=30):
        game = cls(initial_state)
        for _ in range(generations):
            game.tick()
            print(game.state_string)
        return game.current_state

sample_size = 50

sample_state = [
    [False, True, False] + ([False] * (sample_size - 3)),
    [False, False, True] + ([False] * (sample_size - 3)),
    [True, True, True] + ([False] * (sample_size - 3)),
] + [[False] * sample_size for _ in range(sample_size - 3)]


if __name__ == '__main__':
    GameOfLife.run(sample_state)
