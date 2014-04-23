import re


class LeafMatcher(object):

    def __init__(self, regexp):
        self.matcher = re.compile(regexp)

    def matches(self, string):
        return bool(self.matcher.search(string))


class AndMatcher(object):

    def __init__(self, *matchers):
        self.matchers = matchers

    def matches(self, string):
        return all(matcher.matches(string) for matcher in self.matchers)


class OrMatcher(object):

    def __init__(self, *matchers):
        self.matchers = matchers

    def matches(self, string):
        return any(matcher.matches(string) for matcher in self.matchers)