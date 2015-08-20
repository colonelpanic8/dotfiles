def singleton(klass):
    original_init = klass.__dict__.get('__init__')
    class klassMeta(type, klass):

        def __init__(self, *args):
            original_init(self)
            super(klass, self).__init__(*args)

    class Temp(object):
        __metaclass__ = klassMeta
    Temp.__name__ = klass.__name__
    klassMeta.__name__ = "{}Meta".format(klass.__name__)
    return Temp


@singleton
class TestSingleton(object):

    def __init__(self):
        self.a = 22
        self.b = 44

    def hey(self):
        return self.a + self.b
