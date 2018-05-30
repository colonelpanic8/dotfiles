#!/usr/bin/env python
import parse_go_testify_not_equal
import sys
import json

if __name__ == '__main__':
    actual, expected = parse_go_testify_not_equal.get_strings(sys.stdin.read())
    print json.dumps({"actual": actual, "expected": expected})
