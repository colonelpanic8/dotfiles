import functools


def compose2(f, g):
    return lambda x: f(g(x))


def compose(*functions):
    return reduce(compose2, functions)


def extract_args(function):
    @functools.wraps(function)
    def wrapped(args):
        return function(*args)
    return wrapped


def extract_kwargs(function):
    @functools.wraps(function)
    def wrapped(kwargs):
        return function(**kwargs)
    return wrapped


def extract_args_kwargs(function):
    @functools.wraps(function)
    def wrapped((args, kwargs)):
        return function(*args, **kwargs)
    return wrapped
