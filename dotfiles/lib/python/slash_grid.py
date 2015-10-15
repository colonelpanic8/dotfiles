#!/usr/bin/env python
class SlashGrid(object):

    def __init__(self, slash_array):
        self.slash_array = slash_array
        self.nodes = [
            [Node(row_index, column_index, self) for column_index, value in enumerate(row)]
            for row_index, row in enumerate(slash_array)
        ]

    @property
    def width(self):
        return len(self.nodes[0])

    @property
    def height(self):
        return len(self.nodes)

    def run(self, in_between_searches=lambda x: x):
        count = 0
        for node_row in self.nodes:
            for node in node_row:
                if not node.top_visited:
                    in_between_searches(self)
                    count += 1
                    node.search('top', tag=count)
                if not node.bottom_visited:
                    in_between_searches(self)
                    count += 1
                    node.search('bottom', tag=count)
        in_between_searches(self)
        return count

    def __str__(self):
        return '\n'.join(self.grid_row_string(row) for row in self.nodes)

    def grid_row_string(self, row):
        node_strings = [
            node.string_lines for node in row
        ]
        return '\n'.join(''.join(string_collection) for string_collection in zip(*node_strings))


class OutOfBoundsError(Exception):
    pass


class Node(object):

    _right_top = ('right', 'top')
    _right_bottom = ('right', 'bottom')
    _left_top = ('left', 'top')
    _left_bottom = ('left', 'bottom')

    _directions_map = {
        True: (_left_top, _right_bottom),
        False: (_left_bottom, _right_top)
    }

    _opposites = {
        'left': 'right',
        'right': 'left',
        'top': 'bottom',
        'bottom': 'top'
    }

    def __init__(self, row_index, column_index, grid):
        self.row_index = row_index
        self.column_index = column_index
        self.grid = grid
        self.top_visited = False
        self.bottom_visited = False

    @property
    def string_lines(self):
        if self.forward:
            return [''.join([self.string_for_visited(self.top_visited), '/']),
                    ''.join(['/', self.string_for_visited(self.bottom_visited)])]
        else:
            return [''.join(['\\', self.string_for_visited(self.top_visited)]),
                    ''.join([self.string_for_visited(self.bottom_visited), '\\'])]

    @staticmethod
    def string_for_visited(visited):
        if visited is True:
            return 'X'
        elif visited is False:
            return ' '
        else:
            string = str(visited)
            if isinstance(visited, int):
                return str(visited)
            if len(string) > 1:
                return '`'
            else:
                return string

    @property
    def forward(self):
        return self.grid.slash_array[self.row_index][self.column_index]

    def directions_from(self, edge):
        for direction_pair in self._directions_map[self.forward]:
            if edge in direction_pair:
                return direction_pair
        else:
            raise Exception()

    def opposite(self, edge):
        return self._opposites[edge]

    def edge_visited(self, edge):
        return getattr(self, self.edge_visited_string(edge))

    def edge_visited_string(self, edge):
        return '{0}_visited'.format(edge)

    def visit_edge(self, edge, tag):
        was_unvisited = not self.edge_visited(edge)
        if was_unvisited:
            setattr(self, self.edge_visited_string(edge), tag)
        return was_unvisited

    def search(self, edge, tag=True):
        was_unvisited = self.visit_edge(edge, tag)
        if not was_unvisited:
            return
        directions = self.directions_from(edge)
        for travel_edge in directions:
            try:
                getattr(self, travel_edge).search(self.opposite(travel_edge), tag=tag)
            except OutOfBoundsError:
                pass

    @property
    def left_visited(self):
        if self.forward:
            return self.top_visited
        else:
            return self.bottom_visited

    @property
    def right_visited(self):
        if self.forward:
            return self.bottom_visited
        else:
            return self.top_visited

    @right_visited.setter
    def right_visited(self, value):
        if self.forward:
            self.bottom_visited = value
        else:
            self.top_visited = value


    @left_visited.setter
    def left_visited(self, value):
        if self.forward:
            self.top_visited = value
        else:
            self.bottom_visited = value

    @property
    def left(self):
        if self.column_index <= 0:
            raise OutOfBoundsError()
        return self.grid.nodes[self.row_index][self.column_index-1]

    @property
    def right(self):
        if self.column_index > self.grid.width - 2:
            raise OutOfBoundsError()
        return self.grid.nodes[self.row_index][self.column_index + 1]

    @property
    def top(self):
        if self.row_index <= 0:
            raise OutOfBoundsError()
        return self.grid.nodes[self.row_index - 1][self.column_index]

    @property
    def bottom(self):
        if self.row_index > self.grid.height-2:
            raise OutOfBoundsError()
        return self.grid.nodes[self.row_index + 1][self.column_index]


if __name__ == '__main__':
    def print_grid(grid):
        print "X"*100
        print grid

    def square(size, in_between_searches=lambda x: x):
        import random
        sg = SlashGrid([[bool(random.randint(0, 1)) for _ in range(size)] for _ in range(size)])
        size = sg.run(in_between_searches)
        return size
    # SlashGrid([
    #     [True, False, True],
    #     [False, True, True]
    # ]).run(print_grid)
    # SlashGrid([
    #     [True, True, True],
    #     [False, False, False],
    #     [True, False, True]
    # ]).run(print_grid)
    # SlashGrid([
    #     [True, True, True, False, True],
    #     [False, False, False, True, False],
    #     [True, False, True, False, False],
    #     [True, True, True, False, True],
    # ]).run(print_grid)
    # SlashGrid([
    #     [True, True, True, False, True, False],
    #     [False, False, True, False, False, False],
    #     [True, True, True, False, False, False],
    #     [True, False, False, True, True, True],
    #     [True, False, False, True, False, False],
    #     [True, True, True, False, True, False]
    # ]).run(print_grid)
    print square(3, in_between_searches=print_grid)
