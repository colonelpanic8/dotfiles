#!/usr/bin/env python
import sys


def unescape(string):
    print eval(string)


if __name__ == '__main__':
    unescape(sys.stdin.read())
