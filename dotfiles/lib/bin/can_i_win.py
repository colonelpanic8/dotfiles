def memoize(f):
    memo = {}

    def helper(*args):
        args_tuple = tuple(args)
        if args_tuple not in memo:
            memo[args_tuple] = f(*args)
        return memo[args_tuple]

    return helper


def can_i_win(max_value, available_numbers):
    if not isinstance(available_numbers, frozenset):
        available_numbers = frozenset(available_numbers)
    return _can_i_win(max_value, available_numbers, 0)


@memoize
def _can_i_win(max_value, available_numbers, current_value):
    for number in available_numbers:
        new_value = current_value + number
        if new_value > max_value:
            continue
        new_numbers = available_numbers - frozenset([number])
        can_win, _ = _can_i_win(max_value, new_numbers, new_value)
        if not can_win:
            return (True, number)
    return (False, list(iter(available_numbers))[0])


def play_game(max_value, max_number, computer_parity=0):
    available_numbers = frozenset(range(max_number))
    move_count = 0
    count = 0
    while True:
        print("Count is {0}, numbers are {1}, max is {2}".format(
            count, available_numbers, max_value
        ))
        computer_turn = move_count % 2 == computer_parity
        if move_count % 2 == computer_parity:
            can_win, number = _can_i_win(max_value, available_numbers, count)
            print("Computer thinks it can win: {0}".format(can_win))
        else:
            number = get_valid_selection(available_numbers)
        count += number
        available_numbers -= frozenset([number])
        move_count += 1
        print("{0} selected, count is now {1}".format(number, count))
        if count > max_value:
            if computer_turn:
                print("You win")
            else:
                print("You lose")
            return
        print("------------------------------------------------------")


def get_valid_selection(valid_numbers):
    while True:
        number = int(input(
            "Enter one of the following numbers {0}:\n".format(valid_numbers)
        ))
        if number in valid_numbers:
            return number


if __name__ == '__main__':
    play_game(14, 7, computer_parity=1)
