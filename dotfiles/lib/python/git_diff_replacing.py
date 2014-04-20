#!/usr/bin/env python
import argparse

from iterpipes import *


class GitDiffReplacer(object):

    def __init__(self, string_to_replace, replacing_string,
                 source_ref='HEAD~1', destination_ref='HEAD',
                 verbose=False):
        self.source_ref = source_ref
        self.destination_ref = destination_ref
        self.string_to_replace = string_to_replace
        self.replacing_string = replacing_string
        self.verbose = verbose


    @property
    def modified_files_command(self):
        return linecmd('git diff {} {} --name-only', self.source_ref,
                   self.destination_ref)

    def git_diff_command(self, filename):
        return cmd('git diff {}:{} {}:{}', self.source_ref, filename.strip(),
                   self.destination_ref, self.perform_substitutions(filename).strip())

    def perform_substitutions(self, filename):
        return filename.replace(self.string_to_replace, self.replacing_string)

    def filter_filenames(self, filenames):
        for filename in filenames:
            if not self.replacing_string in filename:
                yield filename

    def run(self):
        return '\n'.join([
            list(run(self.git_diff_command(filename)))[0]
            for filename in self.filter_filenames(
                run(self.modified_files_command)
            )
        ])


if __name__ == '__main__':
    import sys
    print GitDiffReplacer(sys.argv[1], sys.argv[2]).run()