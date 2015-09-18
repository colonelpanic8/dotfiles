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


def build_trie():
    node = TrieNode()
    with open('/usr/share/dict/words') as the_file:
        for word in the_file.readlines():
            node.add(word.lower().strip())
    return node


class Boggle(object):

    unit = (1, 0, -1)
    deltas = [(i, j) for i in unit for j in unit]

    @classmethod
    def new_random(cls, trie):
        return cls(build_board(), trie)

    def __init__(self, board, trie):
        self.height = len(board)
        self.width = len(board[0])
        self.board = board
        self.trie = trie

    def run(self):
        for i in range(self.width):
            for j in range(self.height):
                for word in self.find(i, j):
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
        character = self.board[i][j]
        visited.add((i, j))
        if character in trie.children:
            current_word += character
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


if __name__ == '__main__':
    print list(Boggle.new_random(build_trie()))
