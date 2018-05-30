#!/usr/bin/env python
from __future__ import print_function
import appdirs
import argparse
import os
import struct


clipit_history_file = os.path.join(appdirs.user_data_dir(), "clipit/history")


def get_clipit_history(filename):
    with open(filename, 'rb') as f:
        f.read(68)
        size, _ = struct.unpack('2i', f.read(8))
        while (size > 0):
            item = f.read(size)
            if item:
                yield item.decode('utf-8')
            _, _, _, size, _ = struct.unpack('5i', f.read(20))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Lookup clipit history')
    parser.add_argument(
        '--separator', '-s',
        help='the separator to use when outputting history',
        type=str,
        default='\n',
    )
    parser.add_argument(
        '--begin-index', '-b',
        type=int,
        default=0
    )
    parser.add_argument(
        '--end-index', '-e',
        type=int,
        default=None
    )
    parser.add_argument(
        '--index', '-i',
        type=int,
        default=None
    )
    parser.add_argument(
        '--separator-replacement', '-r',
        type=str,
        default=None
    )
    args = parser.parse_args()
    if not args.separator_replacement:
        args.separator_replacement = args.separator
    history = list(get_clipit_history(clipit_history_file))
    if args.index is not None:
        text = history[args.index]
    else:
        selected = history[args.begin_index:args.end_index]
        text = args.separator.join([s.replace(args.separator, args.separator_replacement)
                                    for s in selected])
    print(text, end='')
