class MemoryManager(object):

    def __init__(self, N=1024):
        self._memory = [None for _ in range(1024)]
        self._memory_size = N
        self._last_block = self._memory_size - 1
        self._allocations = []

    def malloc(self, size):
        if size > 5:
            print (size)
        next_startpoint = 0
        minimum_size = None
        minimum_start = None

        for (allocation_start_point, size) in self._allocations:
            current_block_size = allocation_start_point - next_startpoint
            if (current_block_size > size):
                if (minimum_size is None or minimum_size > current_block_size):
                    minimum_size = current_block_size
                    minimum_start = next_startpoint
            next_startpoint = allocation_start_point + size - 1

        current_block_size = self._memory_size - next_startpoint
        if (current_block_size > size):
            if (minimum_size is None or minimum_size > current_block_size):
                minimum_size = current_block_size
                minimum_start = next_startpoint

        if minimum_start is None:
            raise Exception("Could not allocate enough space")

        for (index, (allocation_start, _)) in enumerate(self._allocations):
            if allocation_start > minimum_start:
                self._allocations.insert(index, (minimum_start, size))
                break
        else:
            self._allocations.append((minimum_start, size))

        return minimum_start

    def free(self, pointer):
        for (index, (start_point, _)) in enumerate(self._allocations):
            if pointer == start_point:
                self._allocations.pop(index)
                break
        else:
            raise Exception("Unrecognized pointer")


if __name__ == '__main__':
    mm = MemoryManager()
    allocations = [mm.malloc(5) for _ in range(100)]
    for allocation in allocations[:50]:
        mm.free(allocation)

    print("nice", mm.malloc(200))
    print("cool", mm.malloc(500))
    print(mm._allocations)
    # a = mm.malloc(100)
    # b = mm.malloc(500)
    # print(a, b)
    # print(mm._allocations)
    # mm.free(a)
    # print(mm._allocations)
    # mm.free(b)
    # print(mm._allocations)
    # c = mm.malloc(1000)
    # print(mm._allocations)
