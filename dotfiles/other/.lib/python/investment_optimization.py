#!/usr/bin/env python
import heapq
from collections import namedtuple


BuyOpportunity = namedtuple(
    'BuyOpportunity',
    ['trans_yield', 'start', 'end']
)


def maximize_profit(num_transactions, prices):
    minima, maxima = find_extrema(prices)
    opp_queue, reverse_opp_queue = make_opportunity_queues(
        minima, maxima, prices,
    )
    if not opp_queue:
        return []

    largest_segment = opp_queue[0][1]
    # Segments will be kept in sorted order
    segments = [largest_segment]

    # Remove any reverse yields that are greater than the largest actual yield
    # since they can never be realized anyway.
    while (reverse_opp_queue and reverse_opp_queue[0][1].trans_yield >=
           largest_segment.trans_yield):
        heapq.heappop(reverse_opp_queue)

    def try_rev_opp():
        # It is okay to definitely pop here even though we don't know that we
        # can actually use the opp for the following reason:
        # Since the rev opp queue was selected OVER that of the opp queue, we
        # KNOW that the bounding segment that includes this rev opp must have
        # already been selected if it is going to be included at all (since it
        # must have greater yield).
        _, rev_opp_can = heapq.heappop(reverse_opp_queue)
        for (seg_index, split_seg) in enumerate(segments):
            if split_seg.end >= rev_opp_can.end:
                # Since segments is sorted, this must be the correct segment
                break
        else:
            return
        if split_seg.start <= rev_opp_can.start:
            # We found the containing segment
            left_yield = prices[rev_opp_can.start] - prices[split_seg.start]
            right_yield = prices[split_seg.end] - prices[rev_opp_can.end]
            left_segment = BuyOpportunity(left_yield, split_seg.start, rev_opp_can.start)
            right_segment = BuyOpportunity(right_yield, rev_opp_can.end, split_seg.end)
            segments.pop(seg_index)
            segments.insert(seg_index, left_segment)
            segments.insert(seg_index + 1, right_segment)

    def try_opp():
        _, opp = heapq.heappop(opp_queue)
        if not segments:
            segments.append(opp)
        insertion_index = 0
        for (index, seg) in enumerate(segments):
            if seg.start >= opp.start:
                insertion_index = index
                break
        else:
            insertion_index = len(segments)
            seg = None
        previous_seg = segments[insertion_index - 1] if insertion_index > 0 else None

        if ((seg is None or seg.start >= opp.end) and
            (previous_seg is None or previous_seg.end <= opp.start)):
            # There is no overlap, so we can insert
            segments.insert(insertion_index, opp)
        else:
            pass

    while (opp_queue or reverse_opp_queue) and len(segments) < num_transactions:
        if not reverse_opp_queue:
            try_opp()
        elif not opp_queue:
            try_rev_opp()
        else:
            opp_can = opp_queue[0][1]
            rev_opp_can = reverse_opp_queue[0][1]

            if rev_opp_can.trans_yield > opp_can.trans_yield:
                try_rev_opp()
            else:
                try_opp()

    return segments


def make_opportunity_queues(minima, maxima, prices):
    opp_queue = []
    reverse_opp_queue = []
    for min_index, minimum in enumerate(minima):
        for max_index, maximum in enumerate(maxima):
            transaction_yield = prices[maximum] - prices[minimum]
            if transaction_yield < 0:
                # We can ignore this pair because the transaction has negative
                # yield.
                continue
            # minimum comes before maximum in time
            if minimum < maximum:
                # Transaction yield is made negative because heapq is a min-heap
                heapq.heappush(
                    opp_queue, ((-transaction_yield, maximum - minimum), BuyOpportunity(
                        transaction_yield, minimum, maximum,
                    )),
                )
            else:
                heapq.heappush(
                    reverse_opp_queue, (-transaction_yield, BuyOpportunity(
                        transaction_yield, maximum, minimum,
                    ))
                )
    return opp_queue, reverse_opp_queue


def find_extrema(prices):
    maxima = []
    minima = []
    length_of_prices = len(prices)
    if length_of_prices < 2:
        return minima, maxima

    upwards = None
    last = prices[0]

    for (index, price) in enumerate(prices):
        if price < last:
            if upwards is True:
                maxima.append(index - 1)
            elif upwards is None:
                # We set the starting price as a maximum, but theres no point
                # since we would really never buy.
                maxima.append(0)
                pass
            upwards = False
        elif price > last:
            if upwards is False:
                minima.append(index - 1)
            elif upwards is None:
                # The starting value is a minimum
                minima.append(0)
            upwards = True
        last = price

    if upwards is True:
        maxima.append(length_of_prices - 1)
    elif upwards is False:
        minima.append(length_of_prices - 1)

    return minima, maxima


if __name__ == '__main__':
    print (maximize_profit(10, [0, 1, 3, 2, 3, 0, 10, 12, 1, 2, 3, 2, 0, 2, 4, 3, 6, 4, 14, 1, 0, 2, 4, 5, 4, 5, 6]))

    print [
        BuyOpportunity(trans_yield=1, start=0, end=1),
        BuyOpportunity(trans_yield=12, start=2, end=4),
        BuyOpportunity(trans_yield=2, start=5, end=7),
        BuyOpportunity(trans_yield=6, start=9, end=11),
        BuyOpportunity(trans_yield=6, start=12, end=13),
        BuyOpportunity(trans_yield=10, start=14, end=15),
        BuyOpportunity(trans_yield=5, start=17, end=20),
        BuyOpportunity(trans_yield=2, start=21, end=23),
    ]
