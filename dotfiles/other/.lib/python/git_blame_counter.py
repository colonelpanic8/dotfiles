#!/usr/bin/env python
import optparse
import os
import re
import subprocess


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


def build_file_extension_re(file_extensions):
    return '.*\.(?:' + '|'.join(file_extensions) + ')'


class BlameCounter(object):

    DIVIDER = '------------------------------'
    committer_matcher = re.compile('\((.*?)\s*[0-9]{4}')

    def __init__(
        self,
        search_expressions=(),
        ignore_expressions=(),
        filename_re='.*\.(?:py|tmpl)',
        chunk_size=None,
    ):
        self.path_matchers = [
            re.compile(search_expression)
            for search_expression in search_expressions
        ]
        self.ignore_matchers = [
            re.compile(ignore_expression)
            for ignore_expression in ignore_expressions
        ]
        self.filename_matcher = re.compile(filename_re)
        self.chunk_size = chunk_size
        self.blame_line_count_map = {}

    def match_path_and_filename(self, path, filename):
        filepath = os.path.join(path, filename)
        return all(
            bool(path_matcher.search(filepath)) for path_matcher in self.path_matchers
        ) and bool(self.filename_matcher.search(filename))

    def get_matching_files(self):
        for directory_path, directory_names, filenames in os.walk('.'):
            for directory_name in directory_names:
                if any(
                        ignore_matcher.search(directory_name)
                        for ignore_matcher in self.ignore_matchers
                ):
                    del directory_names[directory_names.index(directory_name)]
            for filename in filenames:
                if self.match_path_and_filename(directory_path, filename):
                    yield os.path.join(directory_path, filename)

    def git_blame_files(self, filenames):
        for filename in filenames:
            if subprocess.call(
                ['git ls-files %s --error-unmatch' % filename],
                shell=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            ):
                continue
            yield (filename, subprocess.Popen(
                ['git', 'blame', filename],
                stdout=subprocess.PIPE
            ).communicate()[0])

    def count_blame_lines(self):
        for blame_output_chunk in segment(
            self.git_blame_files(self.get_matching_files()),
            self.chunk_size
        ):
            self._count_blame_lines(blame_output_chunk)
            if self.chunk_size:
                self.print_results(
                    max_committers=50,
                    min_blame_lines=None
                )

    def _count_blame_lines(self, blame_outputs):
        for _, blame_output in blame_outputs:
            for line in blame_output.split('\n'):
                match = self.committer_matcher.search(line)
                if match:
                    committer = match.group(1)
                    self.blame_line_count_map[committer] = \
                        self.blame_line_count_map.setdefault(committer, 0) + 1

    def get_blame_lines_in_files_by_comitters(self):
        blame_count_in_files_by_committer = {}
        for filename, blame_output in self.git_blame_files(self.get_matching_files()):
            for line in blame_output.split('\n'):
                match = self.committer_matcher.search(line)
                if match:
                    committer = match.group(1)
                    committer_blame_lines = blame_count_in_files_by_committer.setdefault(
                        committer, {},
                    )
                    committer_blame_lines[filename] = committer_blame_lines.setdefault(
                        filename, 0,
                    ) + 1
        return blame_count_in_files_by_committer

    def print_results(self, max_committers=None, min_blame_lines=None):
        print self.DIVIDER
        for (rank, (committer, blame_lines)) in enumerate(
            sorted(
                self.blame_line_count_map.iteritems(),
                key=lambda x: x[1],
                reverse=True
            )
        ):
            if rank is not None and rank == max_committers:
                return
            if min_blame_lines is None or blame_lines > min_blame_lines:
                print str(rank + 1), committer, ': ', blame_lines


if __name__ == '__main__':
    parser = optparse.OptionParser()
    parser.add_option(
        '--search-re',
        action='append',
        dest='search_expressions',
        help='A regular expression to use when inspecting filepaths'
    )
    parser.add_option(
        '--ignore-re',
        action='append',
        default=[],
        dest='ignore_expressions',
        help='Ignore directories matching this re.'
    )
    parser.add_option(
        '-x',
        action='append',
        dest='file_extensions',
        help=('Search for filenames with the given file extension. '
              'Can be used multiple times.')
    )
    parser.add_option(
        '--chunk-size',
        dest='chunk_size',
        type=int,
        help='Print the rankings at intervals of CHUNK_SIZE files.'
    )
    parser.add_option(
        '--committer-lines',
        dest='committer_lines',
        action='store_true',
        default=False,
        help=('Count blame lines for committer by file.')
    )

    (namespace, _) = parser.parse_args()

    blame_counter_build_kwargs = {
        'chunk_size': namespace.chunk_size,
        'search_expressions': namespace.search_expressions,
        'ignore_expressions': namespace.ignore_expressions
    }
    if namespace.file_extensions:
        blame_counter_build_kwargs['filename_re'] = build_file_extension_re(
            namespace.file_extensions
        )

    blame_counter = BlameCounter(**blame_counter_build_kwargs)
    if namespace.committer_lines:
        import operator

        def sum_of_comitter_lines(committer_tuple):
            _, blame_lines_by_file = committer_tuple
            return sum(blame_count for filename, blame_count in blame_lines_by_file.iteritems())
        blame_lines_in_files_by_committers = blame_counter.get_blame_lines_in_files_by_comitters()
        blame_lines_in_files_by_comitters_sorted_by_total_count = sorted(
            blame_lines_in_files_by_committers.iteritems(),
            key=sum_of_comitter_lines,
            reverse=True
        )
        sorted_blame_lines_in_files_by_comitters = [
            (comitter, sorted(blame_lines_by_file.iteritems(), key=operator.itemgetter(1), reverse=True))
            for comitter, blame_lines_by_file in blame_lines_in_files_by_comitters_sorted_by_total_count
        ]
        import ipdb; ipdb.set_trace()
    else:
        blame_counter.count_blame_lines()
        blame_counter.print_results()
