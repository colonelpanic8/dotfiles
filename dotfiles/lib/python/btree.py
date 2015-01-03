import bisect


class BTreePrinter(object):

    number_width = 4

    subtree_space = object()

    def __init__(self, btree):
        self.btree = btree

    def determine_width_of_node(self, node):
        if node is None: return 0
        return sum(map(self.determine_width_of_node, node.nodes)) + node.number_of_value_nodes * self.number_width

    def determine_width_of_value_node(self, value_node):
        return self.determine_width_of_node(value_node.node) if value_node.node is not None else self.number_width

    def print_tree(self):
        return self.print_levels_recursively([self.btree.head])

    def print_levels_recursively(self, level):
        if all(map(lambda x: x is self.subtree_space, level)): return
        self.print_nodes_at_level(level)
        print ''
        self.print_levels_recursively(self.get_next_level(level))

    def get_next_level(self, level):
        new_level = []
        for item in level:
            if item is self.subtree_space:
                new_level.append(item)
            elif item is not None:
                new_level.extend(item.nodes)
                new_level.append(self.subtree_space)

        return new_level

    def print_nodes_at_level(self, level):
        for item in level:
            if item is self.subtree_space:
                print ' ' * self.number_width,
            else:
                self.print_values_for_node(item)

    def print_values_for_node(self, node):
        if node is None: return
        for value_node in node.value_nodes:
            print ' ' * self.determine_width_of_node(value_node.node),
            print '{num: ^{width}}'.format(num=value_node.value, width=self.number_width),
        print (' ' * (self.determine_width_of_node(node.rightmost_node))),


class IntegrityChecker(object):

    def __init__(self, btree):
        self.btree = btree

    def check_integrity(self):
        return self.check_for_items_smaller_in_right_subtree(self.btree.head) and self.check_for_unmatched_parents(self.btree.head)

    def check_for_unmatched_parents(self, subtree):
        if subtree is None:
            return True

        for node in subtree.nodes:
            if node is None:
                continue
            if node.parent is not subtree:
                return False
            if not self.check_for_unmatched_parents(node):
                return False
        return True

    def check_for_items_smaller_in_right_subtree(self, subtree):
        if subtree is None:
            return True

        small_value = subtree.value_nodes[0].value
        for value_node in subtree.value_nodes[1:]:
            if not self.check_subtree_has_no_items_smaller_than(value_node.node, small_value):
                return False

            if not self.check_for_items_smaller_in_right_subtree(subtree.value_nodes[0].node):
                return False

        return self.check_subtree_has_no_items_smaller_than(subtree.rightmost_node, small_value)

    def check_subtree_has_no_items_smaller_than(self, subtree, value):
        if subtree is None:
            return True
        for value_node in subtree.value_nodes:
            if value > value_node.value:
                return False
            if not self.check_subtree_has_no_items_smaller_than(value_node.node, value):
                return False
        return self.check_subtree_has_no_items_smaller_than(subtree.rightmost_node, value)


class BTree(object):

    @classmethod
    def build_with_value(cls, value):
        btree = cls()
        btree.head = Node(btree, [ValueNode(value)])
        return btree

    def __init__(self):
        self.head = None
        self.inserted_items = []

    def build_new_head(self, value_node):
        new_rightmost_node = self.head
        self.head = Node(self, [value_node])
        value_node.node.parent = self.head
        self.head.rightmost_node = new_rightmost_node
        new_rightmost_node.parent = self.head
        assert self.head.rightmost_node is not None
        return value_node

    def insert(self, value):
        self.head.insert(value)
        self.inserted_items.append(value)
        self.head.check_integrity()
        if not IntegrityChecker(self).check_integrity():
            import ipdb; ipdb.set_trace()

    promote_value_node = build_new_head

    def __repr__(self):
        return "BTree({0})".format(repr(self.head))


class ValueNode(object):

    def __init__(self, value, node=None):
        self.value = value
        self.node = node

    def __lt__(self, other):
        return self.value < other.value

    def __gt__(self, other):
        return self.value > other.value

    def __repr__(self):
        return "ValueNode({0}, {1})".format(repr(self.node), repr(self.value))


class Node(object):

    max_num_values = 3

    def __init__(self, parent, value_nodes=None, rightmost_node=None):
        self.parent = parent
        self.value_nodes = value_nodes or []
        self.rightmost_node = rightmost_node
        self.claim_child_nodes()

    def check_integrity(self):
        if self.is_leaf_node: return True
        if self.rightmost_node:
            return all(child_node.check_integrity() for child_node in self.nodes if child_node is not None)
        import ipdb; ipdb.set_trace()
        return False

    def claim_child_nodes(self):
        for node in self.nodes:
            if node:
                node.parent = self

    @property
    def is_leaf_node(self):
        return not any(self.nodes)

    @property
    def number_of_value_nodes(self):
        return len(self.value_nodes)

    @property
    def nodes(self):
        return [value_node.node for value_node in self.value_nodes] + [self.rightmost_node]

    @property
    def values(self):
        return [value_node.value for value_node in self.value_nodes]

    def __getitem__(self, item):
        return self.nodes[item]

    def promote_value_node(self, value_node):
        bisect.insort(self.value_nodes, value_node)
        if value_node.node:
            value_node.node.parent = self
        self.maybe_rebalance()

    def maybe_rebalance(self):
        if self.number_of_value_nodes < self.max_num_values:
            return
        value_node_to_promote = self.value_nodes[self.number_of_value_nodes/2]
        promoted_nodes_old_node = value_node_to_promote.node
        value_node_to_promote.node = Node(
            self.parent,
            value_nodes=self.value_nodes[:self.number_of_value_nodes/2],
            rightmost_node=promoted_nodes_old_node
        )
        self.value_nodes = self.value_nodes[self.number_of_value_nodes/2+1:]
        self.parent.promote_value_node(value_node_to_promote)
        self.check_integrity()

    def insert(self, value):
        if self.is_leaf_node:
            value_node = ValueNode(value)
            bisect.insort(self.value_nodes, value_node)
            self.maybe_rebalance()
            return value_node

        return self.pick_node(value).insert(value)

    def pick_node(self, value):
        if self.rightmost_node is None:
            import ipdb; ipdb.set_trace()
        for value_node in self.value_nodes:
            if value < value_node.value:
                return value_node.node

        return self.rightmost_node

    def __repr__(self):
        return "Node({0}, {1})".format(", ".join(map(repr, self.value_nodes)), self.rightmost_node)