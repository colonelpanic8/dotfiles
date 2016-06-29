#!/usr/bin/env python
import re
import sys
import tempfile
from subprocess import call


expected_re = re.compile("Error:\\s*Not equal:\\s*(.*?)\\s*\\(expected\\)")
actual_re = re.compile("!=\\s*(.*?)\\s*\(actual\)")


def get_strings(incoming):
    expected_match = expected_re.search(incoming)
    actual_match = actual_re.search(incoming)
    return (eval(expected_match.group(1)), eval(actual_match.group(1)))


if __name__ == '__main__':
    stdin = sys.stdin.read()
    _, expected_filename = tempfile.mkstemp()
    _, actual_filename = tempfile.mkstemp()
    expected_text, actual_text = get_strings(stdin)
    with open(expected_filename, 'w') as expected_file:
        expected_file.write(expected_text)

    with open(actual_filename, 'w') as actual_file:
        actual_file.write(actual_text)

    call(["icdiff", expected_filename, actual_filename, "--show-all-spaces"])
