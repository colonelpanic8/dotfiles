#!/usr/bin/env python
from collections import namedtuple
import argparse
import re

from iterpipes import *


CommitInfo = namedtuple('CommitInfo', ['hash', 'author', 'email'])


class AlwaysMatches(object):

    @classmethod
    def matches(*args):
        return True


class GitAuthorReplacer(object):

    def __init__(self, author_string, email_string,
                 source_branch='HEAD', commit_matcher=AlwaysMatches):
        self.author_string = author_string
        self.email_string = email_string
        self.source_branch = source_branch
        self.commit_matcher = commit_matcher

    @property
    def log_triples(self):
        return map(self.build_hash_author_email,
                   run(linecmd('git log --pretty=format:"%h|%an|%ae"')))

    @staticmethod
    def build_hash_author_email(line):
        line = line.strip()
        print line
        print line.split('|')
        return CommitInfo(*line.split('|'))

    def recredit_commits(self):
        reversed_triples = self.log_triples[::-1]
        self.reset(reversed_triples[0])
        self.conditionally_recredit_commit(reversed_triples[0])
        for commit_info in reversed_triples[1:]:
            self.cherry_pick(commit_info)
            self.conditionally_recredit_commit(commit_info)

    @staticmethod
    def reset(commit_info):
        return list(run(cmd('git reset --hard {}', commit_info.hash)))

    @staticmethod
    def cherry_pick(commit_info):
        return list(run(cmd('git cherry-pick {}', commit_info.hash)))

    def conditionally_recredit_commit(self, commit_info):
        if self.commit_matcher.matches(commit_info):
            return list(
                run(cmd('git commit --amend --author "{} {}" -C HEAD', self.author_string, '<{0}>'.format(self.email_string)))
            )


class NameMatcher(object):

    def __init__(self, name_regexp):
        self.matcher = re.compile(name_regexp)

    def matches(self, commit_info):
        return bool(self.matcher.search(commit_info.author))


class AndMatcher(object):

    def __init__(self, *matchers):
        self.matchers = matchers

    def matches(self, commit_info):
        return all(matcher.matches(commit_info) for matcher in self.matchers)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '--name',
        action='store',
        type=str,
        dest='name',
        default='',
        help='Name to set for matching commits.',
    )
    parser.add_argument(
        '--email',
        action='store',
        type=str,
        dest='email',
        default='',
        help='Email to set for matching commits.'
    )
    parser.add_argument(
        '--name-re',
        action='append',
        dest='name_regexps',
        help='A regular expression to use when inspecting filepaths'
    )
    namespace = parser.parse_args()
    name_matchers = map(NameMatcher, namespace.name_regexps)
    GitAuthorReplacer(namespace.name, namespace.email, commit_matcher=AndMatcher(*name_matchers)).recredit_commits()