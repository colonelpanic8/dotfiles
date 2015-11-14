#!/usr/bin/env python
import sys


def escape(string):
    print repr(string)


if __name__ == '__main__':
    escape(sys.stdin.read())
