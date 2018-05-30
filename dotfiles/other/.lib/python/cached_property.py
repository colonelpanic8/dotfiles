import inspect


class cached_property(object):
    """Descriptor that caches the result of the first call to resolve its
    contents.
    """

    def __init__(self, func):
        self.__doc__ = getattr(func, '__doc__')
        self.func = func

    def __get__(self, obj, cls):
        if obj is None:
            return self
        value = self.func(obj)
        setattr(obj, self.func.__name__, value)
        return value

    def bust_self(self, obj):
        """Remove the value that is being stored on `obj` for this
        :class:`.cached_property`
        object.

        :param obj: The instance on which to bust the cache.
        """
        if self.func.__name__ in obj.__dict__:
            delattr(obj, self.func.__name__)

    @classmethod
    def bust_caches(cls, obj, excludes=()):
        """Bust the cache for all :class:`.cached_property` objects on `obj`

        :param obj: The instance on which to bust the caches.
        """
        for name, _ in cls.get_cached_properties(obj):
            if name in obj.__dict__ and name not in excludes:
                delattr(obj, name)

    @classmethod
    def get_cached_properties(cls, obj):
        return inspect.getmembers(type(obj), lambda x: isinstance(x, cls))
