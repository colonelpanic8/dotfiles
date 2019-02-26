class MazeSolver(object):

    def __init__(self, maze):
        self.maze = maze
        self.row_length = len(maze[0])
        self.column_length = len(maze)
        self.visited = set()

    @property
    def finish(self):
        return (self.column_length - 1, self.row_length - 1)

    deltas = [(1, 0), (0, 1), (-1, 0), (0, -1)]

    def is_in_bounds(self, location):
        column_index, row_index = location
        return (
            0 <= column_index < self.column_length and
            0 <= row_index < self.row_length
        )

    def find_adjacency(self, location):
        for delta in self.deltas:
            column_delta, row_delta = delta
            column_location, row_location = location
            new_column_location = column_location + column_delta
            new_row_location = row_location + row_delta
            adjacent_location = (new_column_location, new_row_location)
            if (
                    self.is_in_bounds(adjacent_location) and
                    self.maze[new_column_location][new_row_location]
            ):
                yield adjacent_location

    def solve(self, current_location=(0, 0)):
        if current_location == self.finish:
            return [current_location]
        self.visited.add(current_location)
        for new_location in self.find_adjacency(current_location):
            if new_location in self.visited:
                continue
            result = self.solve(new_location)
            if result is not None:
                return [current_location] + result
        return None

if __name__ == '__main__':
    maze = [
        [1, 1, 1, 1, 1],
        [0, 0, 1, 0, 0],
        [1, 0, 1, 1, 1],
        [1, 0, 0, 0, 1],
        [1, 1, 1, 1, 1]
    ]

    print(MazeSolver(maze).solve())
