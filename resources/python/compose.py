import collections
import functools

from partialable import n_partialable


def _compose2(f, g):
    return lambda *args, **kwargs: f(g(*args, **kwargs))


@n_partialable(evaluation_checker=n_partialable.count_evaluation_checker(2))
def compose_with_joiner(joiner, *functions):
    return reduce(joiner, functions)


compose_one_arg = compose_with_joiner(_compose2)


compose = compose_with_joiner(lambda f, g: _compose2(make_single_arity(f),
                                                force_args_return(g)))


def make_single_arity(function):
    @functools.wraps(function)
    def wrapped(args):
        return function(*args)
    return wrapped


def kwargs_make_single_arity(function):
    @functools.wraps(function)
    def wrapped(kwargs):
        return function(**kwargs)
    return wrapped


def args_kwargs_make_single_arity(function):
    @functools.wraps(function)
    def wrapped((args, kwargs)):
        return function(*args, **kwargs)
    return wrapped


def force_args_return(function):
    @functools.wraps(function)
    def wrapped(*args, **kwargs):
        value = function(*args, **kwargs)
        if not isinstance(value, collections.Iterable):
            value = (value,)
        return value
    return wrapped


def tee(*functions):
    def wrapped(*args, **kwargs):
        return tuple(function(*args, **kwargs) for function in functions)
    return wrapped
