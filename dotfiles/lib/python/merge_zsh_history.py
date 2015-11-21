import re


history_entry_re = re.compile(": ([0-9]*):[0-9]*;.*")


class dedup(set):

    def contains_add(self, elem):
        if elem in self:
            return True
        else:
            self.add(elem)
            return False


def merge_history(filenames):
    entries = dedup()
    pairs = [entry for filename in filenames for entry in entry_timestamp_pairs(filename)]
    return "".join([
        p[0] for p in sorted(pairs, key=lambda pair: pair[1])
        if not entries.contains_add(p[0])
    ])


def entry_timestamp_pairs(filename):
    pairs = []
    with open(filename, 'r') as file:
        entry = None
        timestamp = None
        for line in file.readlines():
            if line[0] == ":":
                if entry != None:
                    pairs.append((entry, timestamp))
                timestamp = timestamp_from_line(line)
                entry = line
            else:
                entry += line
    return pairs


def timestamp_from_line(line):
    return int(history_entry_re.search(line).group(1))


if __name__ == '__main__':
    import sys
    print merge_history(sys.argv[1:])
