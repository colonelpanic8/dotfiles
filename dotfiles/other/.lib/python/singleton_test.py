from . import singleton


def test_singleton():
    @singleton.singleton
    class TestSingleton(object):

        def __init__(self):
            self.a = 22
            self.b = 44

        def hey(self):
            return self.a + self.b
    assert TestSingleton.a == 22
    assert TestSingleton.hey() == 66
