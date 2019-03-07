class Node(object):
    def __init__(self, key, value, next_node=None, prev_node=None):
        self.key = key
        self.value = value
        self.next_node = next_node
        self.prev_node = prev_node

    def print_list(self):
        print("{0} - {1}".format(self.key, self.value))
        if self.next_node is not None:
            assert self == self.next_node.prev_node
            self.next_node.print_list()
        else:
            print("next node is None")
        if self.prev_node is not None:
            assert self.prev_node.next_node == self


class LRUCache(object):
    def __init__(self, capacity):
        self.capacity = capacity
        self.cache = {}
        self.head = None
        self.tail = None

    def put(self, key, value):
        """
        If key already exists, replace the current value with the new value.
        If the key doesn't exist, add the new key/value entry to the cache.
        If the addition of the new entry causes the number of entries to exceed
        num_entries, remove the oldest entry based on the last time the entry is
        accessed (either through put or get).
        """
        if key in self.cache:
            node = self.cache[key]
            node.value = value
            self.move_to_tail(node)
            return

        if len(self.cache) >= self.capacity:
            old_head = self.remove_from_head()
            del self.cache[old_head.key]

        new_node = Node(key, value)
        self.set_new_tail(new_node)
        self.cache[key] = new_node

    def set_new_tail(self, node):
        node.prev_node = self.tail
        if self.tail is not None:
            self.tail.next_node = node
        self.tail = node
        if self.head is None:
            self.head = node

    def move_to_tail(self, node):
        if node is self.tail:
            return
        if node.prev_node is None: # This is the head
            if node.next_node is not None:
                self.head = node.next_node
                node.next_node.prev_node = None
        else:
            node.prev_node.next_node = node.next_node
            node.next_node.prev_node = node.prev_node

        node.prev_node = self.tail
        self.tail.next_node = node
        self.tail = node
        self.tail.next_node = None

    def remove_from_head(self):
        previous_head = self.head
        self.head = self.head.next_node
        self.head.prev_node = None
        return previous_head

    def get(self, key):
        """Return the value associated with the key, or None if the key doesn't
        exist."""
        node = self.cache.get(key)
        if node is not None:
            self.move_to_tail(node)
            return node.value
