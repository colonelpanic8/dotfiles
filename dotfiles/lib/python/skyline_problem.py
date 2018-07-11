import heapq


def get_skyline(buildings):
    result = []
    active_buildings = []

    last_index = -1

    def add_entry(index, height):
        last_height = result[-1][1] if result else 0
        if height != last_height:
            result.append([index, height])

    def handle_next_active_building():
        (negative_height, end_index) = heapq.heappop(active_buildings)
        while active_buildings and active_buildings[0][1] <= end_index:
            heapq.heappop(active_buildings)
        new_height = -active_buildings[0][0] if active_buildings else 0
        add_entry(end_index, new_height)

    def add_entry_for_last():
        if active_buildings:
            add_entry(last_index, -active_buildings[0][0])

    for (left_index, right_index, height) in buildings:

        if last_index > -1 and left_index != last_index:
            # We have to do this here inside this if statement to handle the
            # case where multiple building (potentially having different
            # heights) start on the same index.
            add_entry_for_last()

        while active_buildings and active_buildings[0][1] < left_index:
            handle_next_active_building()

        heapq.heappush(active_buildings, (-height, right_index))
        last_index = left_index

    add_entry_for_last()

    while active_buildings:
        handle_next_active_building()

    return result


if __name__ == '__main__':
    print(get_skyline([[1,9,10],[1,7,15],[5,12,12],[15,20,10],[19,24,8], [26, 100, 100]]))
