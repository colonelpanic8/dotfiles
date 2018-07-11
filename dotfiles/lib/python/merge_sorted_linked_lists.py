import heapq

class ListNode(object):
    def __init__(self, x):
        self.val = x
        self.next = None

def merge_sorted_linked_lists(lists):
    min_queue = []
    result = ListNode(None)
    current_node = result
    for list_index, alist in enumerate(lists):
        if alist is None:
            continue
        heap_value = (alist.val, list_index)
        lists[list_index] = alist.next
        heapq.heappush(min_queue, heap_value)

    while min_queue:
        value, list_index = heapq.heappop(min_queue)
        selected_list = lists[list_index]
        if selected_list is not None:
            lists[list_index] = selected_list.next
            heapq.heappush(min_queue, (selected_list.val, list_index))
        new_node = ListNode(value)
        current_node.next = new_node
        current_node = new_node
    return result.next


def make_linked_list(*values):
    result = None
    for value in values[::-1]:
        new_node = ListNode(value)
        new_node.next = result
        result = new_node
    return result


def linked_list_to_list(ll):
    result = []
    while ll:
        result.append(ll.val)
        ll = ll.next
    return result


if __name__ == '__main__':
    lists = [
        make_linked_list(1, 10, 20, 21),
        make_linked_list(16, 17, 18, 22),
        make_linked_list(12, 13, 24),
        make_linked_list(-1, 15, 24),
        make_linked_list(-4),
        None,
    ]
    res = merge_sorted_linked_lists(lists)
    print (linked_list_to_list(res))

