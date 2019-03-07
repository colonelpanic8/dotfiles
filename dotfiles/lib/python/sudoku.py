goodpuzzle = [
    [1,2,3,4,5,6,7,8,9],
    [4,5,6,7,8,9,1,2,3],
    [7,8,9,1,2,3,4,5,6],
    [2,3,4,5,6,7,8,9,1],
    [5,6,7,8,9,1,2,3,4],
    [8,9,1,2,3,4,5,6,7],
    [3,4,5,6,7,8,9,1,2],
    [6,7,8,9,1,2,3,4,5],
    [9,1,2,3,4,5,6,7,8]
]

badpuzzle1 = [
    [1,2,3,4,5,6,7,9,8],
    [4,5,6,7,8,9,1,2,3],
    [7,8,9,1,2,3,4,5,6],
    [2,3,4,5,6,7,8,9,1],
    [5,6,7,8,9,1,2,3,4],
    [8,9,1,2,3,4,5,6,7],
    [3,4,5,6,7,8,9,1,2],
    [6,7,8,9,1,2,3,4,5],
    [9,1,2,3,4,5,6,7,8]
]

badpuzzle2 = [
    [1,2,3,4,5,6,7,2,9],
    [4,5,6,7,8,9,1,8,3],
    [7,8,9,1,2,3,4,5,6],
    [2,3,4,5,6,7,8,9,1],
    [5,6,7,8,9,1,2,3,4],
    [8,9,1,2,3,4,5,6,7],
    [3,4,5,6,7,8,9,1,2],
    [6,7,8,9,1,2,3,4,5],
    [9,1,2,3,4,5,6,7,8]
]

badpuzzle3 = [
    [1,2,3,4,5,6,7,8,9],
    [4,5,6,7,8,9,1,2,3],
    [7,8,9,1,2,3,4,5,6],
    [2,3,4,5,6,7,8,9,1],
    [5,6,7,8,9,1,2,3,4],
    [3,4,5,6,7,8,9,1,2],
    [8,9,1,2,3,4,5,6,7],
    [6,7,8,9,1,2,3,4,5],
    [9,1,2,3,4,5,6,7,8]
]


one_to_nine = set(range(1, 10))


def is_valid_sudoku_puzzle(sudoku_grid):
    for row in sudoku_grid:
        if set(row) != one_to_nine:
            return False

    for i in range(9):
        column = [sudoku_grid[j][i] for j in range(9)]
        if set(column) != one_to_nine:
            return False

    for i in range(3):
        for j in range(3):
            subgrid_elements = get_subgrid_elements(i, j, sudoku_grid)
            if set(subgrid_elements) != one_to_nine:
                return False

    return True


def get_subgrid_elements(subgrid_row, subgrid_column, sudoku_grid, subgrid_size=3):
    subgrid_row_start = subgrid_row * subgrid_size
    subgrid_column_start = subgrid_column * subgrid_size

    subgrid_elements = []
    for i in range(subgrid_row_start, subgrid_row_start + subgrid_size):
        subgrid_elements += sudoku_grid[i][subgrid_column_start:subgrid_column_start+subgrid_size]

    return subgrid_elements


print(is_valid_sudoku_puzzle(goodpuzzle))
print(is_valid_sudoku_puzzle(badpuzzle1))
print(is_valid_sudoku_puzzle(badpuzzle2))
print(is_valid_sudoku_puzzle(badpuzzle3))
