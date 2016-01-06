#!/usr/bin/env python
import argparse
import os


class PathList(object):

    @classmethod
    def from_string(cls, path_string, separator=':'):
        non_empty_paths = [path for path in path_string.split(separator) if path]
        return cls(non_empty_paths, separator=':')

    def __init__(self, paths, separator=':'):
        self.paths = paths
        self.separator = separator

    def __str__(self):
        return self.with_separator(self.separator)

    def with_separator(self, separator=None):
        separator = separator or self.separator
        deduped = []
        included = set()
        for path in self.paths:
            normalized = os.path.normpath(path)
            if normalized not in included:
                included.add(normalized)
                deduped.append(path)
        return separator.join(
            os.path.normpath(path) for path in deduped
        )

    def add(self, new_paths, after=False, target=None):
        if target:
            target_index = self.paths.index(target)
        else:
            target_index = 0

        if after:
            increment = 1 if target else len(self.paths)
            target_index += increment

        self.paths = self.paths[:target_index] + new_paths + self.paths[target_index:]


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Manipulate path variables.')
    parser.add_argument(
        'paths',
        metavar='PATH',
        type=str,
        nargs='*',
        help='paths to add',
    )
    parser.add_argument(
        '--path-var',
        help='the path var to add to.',
        default='PATH',
    )
    parser.add_argument(
        '--separator',
        help='the separator of the path variable',
        default=':',
    )
    parser.add_argument(
        '--path-string',
        help='the path string to edit',
        default=None,
    )
    parser.add_argument(
        '--after',
        help=('whether to do the action after the target (if target is specified)'
              'or the entire path variable'),
        action='store_true',
        default=True,
    )
    parser.add_argument(
        '--before',
        help='inverse of after',
        dest='after',
        action='store_false',
    )
    parser.add_argument(
        '--target',
        help='the target path',
        default=None
    )
    parser.add_argument(
        '--include-assignment',
        action='store_true',
        help='include the assignment command in output',
    )
    parser.add_argument(
        '--print-separator',
        help='separator to use for output',
        default=None,
    )
    parser.add_argument(
        '--path-lines',
        help='use newlines to separate path output',
        action='store_true',
        default=False,
    )
    args = parser.parse_args()

    path_string = args.path_string or os.environ.get(args.path_var, '')
    path_list = PathList.from_string(path_string, separator=args.separator)
    path_list.add(args.paths, after=args.after, target=args.target)

    output_separator = '\n' if args.path_lines else args.print_separator
    output_path = path_list.with_separator(separator=output_separator)
    if args.include_assignment:
        output = "export {}='{}'".format(args.path_var, output_path)
    else:
        output = output_path

    print(output)
