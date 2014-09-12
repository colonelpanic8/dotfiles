import compose


def test_compose_handles_multiple_argument_output_and_non_iterable_output():
    assert compose.compose(lambda x: x*2,
                           lambda x, y: x + y,
                           lambda x, y, z: (2*(x - y), z))(1, 2, 3) == 2


def test_tee():
    assert compose.compose(lambda x, y: x + y,
                           compose.tee(lambda x: x + 1, lambda x: x - 1))(2) == 4
