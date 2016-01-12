def segment(iterable, segment_length):
    if segment_length is None:
        yield iterable
        raise StopIteration

    def yield_length():
        for _ in xrange(segment_length):
            yield iterable.next()
    while True:
        segment = list(yield_length())
        if not segment:
            raise StopIteration
        yield segment
