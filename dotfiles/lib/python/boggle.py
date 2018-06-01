import random


class TrieNode(object):

    def __init__(self, children=None, is_word=False, word=''):
        self.children = children or {}
        self.is_word = is_word
        self.word = word

    def has_word(self, word):
        node, rem = self.get(word)
        if rem:
            return False
        return node.is_word

    def get(self, suffix):
        if not suffix:
            return self, suffix
        character = suffix[0]
        if character in self.children:
            node = self.children[character]
            return node.get(suffix[1:])
        return self, suffix

    def add(self, word):
        node, suffix = self.get(word)
        if not suffix:
            node.is_word = True
            return node
        character = suffix[0]
        new_node = type(self)(word=node.word+character)
        node.children[character] = new_node
        new_node.add(suffix[1:])
        return new_node


def build_board(size=4):
    return [
        [chr(random.randint(97, 97+25)) for i in range(size)]
        for j in range(size)
    ]


def board_string(board_to_draw):
    border_line = "+{0}+".format((len(board_to_draw[0]) * 2 - 1) * "-")
    return "{border_line}\n{contents}\n{border_line}".format(
        contents="\n".join("|{0}|".format(" ".join(letter_line))
                           for letter_line in board_to_draw),
        border_line=border_line,
    )


def build_trie():
    node = TrieNode()
    with open('/usr/share/dict/words') as the_file:
        for word in the_file.readlines():
            node.add(word.lower().strip())
    return node


unit = (1, 0, -1)
class Boggle(object):

    deltas = [(i, j) for i in unit for j in unit]

    @classmethod
    def new_random(cls, trie):
        return cls(build_Board(), trie)

    def __init__(self, board, trie):
        self.height = len(board)
        self.width = len(board[0])
        self.board = board
        self.trie = trie

    def run(self):
        for i in range(self.width):
            for j in range(self.height):
                for word in self.find(i, j):
                    if len(word) > 2:
                        yield word

    def adjacency(self, i, j):
        for i_d, j_d in self.deltas:
            new_i = i_d + i
            new_j = j_d + j
            if 0 <= new_i < self.height and 0 <= new_j < self.width:
                yield new_i, new_j

    def find(self, i, j, trie=None, visited=None, current_word=''):
        trie = trie or self.trie
        visited = visited or set()
        visited = set(visited)
        characters = self.board[i][j]
        visited.add((i, j))
        new_trie = trie
        for character in characters:
            if character in new_trie.children:
                new_trie = new_trie.children[character]
            else:
                break
        else:
            current_word += characters
            new_trie = trie.children[character]
            if new_trie.is_word:
                yield current_word
            for new_i, new_j in self.adjacency(i, j):
                if (new_i, new_j) in visited:
                    continue
                new_visited = set(visited)
                for word in self.find(new_i, new_j, trie=new_trie,
                                      visited=new_visited, current_word=current_word):
                    yield word


boggle_dice = [
    ["a", "a", 'e', 'e', 'g',  'n'],
    ["e", "l", 'r', 't', 't',  'y'],
    ["a", "o", 'o', 't', 't',  'w'],
    ["a", "b", 'b', 'j', 'o',  'o'],
    ["e", "h", 'r', 't', 'v',  'w'],
    ["c", "i", 'm', 'o', 't',  'u'],
    ["d", "i", 's', 't', 't',  'y'],
    ["e", "i", 'o', 's', 's',  't'],
    ["d", "e", 'l', 'r', 'v',  'y'],
    ["a", "c", 'h', 'o', 'p',  's'],
    ["h", "i", 'm', 'n', 'qu', 'u'],
    ["e", "e", 'i', 'n', 's',  'u'],
    ["e", "e", 'g', 'h', 'n',  'w'],
    ["a", "f", 'f', 'k', 'p',  's'],
    ["h", "l", 'n', 'n', 'r',  'z'],
    ["d", "e", 'i', 'l', 'r',  'x'],
]


def random_permutation_of_size(n):
    remaining = list(range(n))
    for i in range(n-1, -1, -1):
        yield remaining.pop(random.randint(0, i))


def random_permutation(the_list):
    for index in random_permutation_of_size(len(the_list)):
        yield the_list[index]


def chunks(l, n):
    for i in range(0, len(l), n):
        yield l[i:i + n]


def roll_dice(dice):
    for die in random_permutation(dice):
        yield random.choice(die)


def build_board_from_dice_roll(dice=None, row_size=4):
    dice = dice or boggle_dice
    return list(chunks(list(roll_dice(dice)), row_size))


if __name__ == '__main__':
    dict_trie = build_trie()
    board = build_board_from_dice_roll()
    print(board_string(board))
    print(list(Boggle(board, dict_trie).run()))
