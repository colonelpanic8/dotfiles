#!/usr/bin/env python
from optparse import OptionParser
from subprocess import Popen, PIPE
import select
import sys
import time


class IntervallicCommandRunner(object):

    def __init__(self, command, command_interval, sleep_time=1):
        self.command = command
        self.command_interval = command_interval
        self.sleep_time = sleep_time
        self.last_time = None
        self.read_last_time = True

    @property
    def can_read_from_stdin(self):
        return sys.stdin in select.select([sys.stdin], [], [], 0)[0]

    def _accumulate_input(self):
        time_to_stop_after = self.last_time + self.command_interval
        lines = []
        new_time = time.time()

        while new_time < time_to_stop_after:
            for _ in range(10):
                if self.can_read_from_stdin:
                    lines.append(sys.stdin.readline())
                    break
            else:
                time.sleep(self.sleep_time)
            new_time = time.time()

        self.last_time = new_time

        return ''.join(lines)

    def loop_indefinitely(self):
        self.last_time = time.time()
        while True:
            Popen([self.command], shell=True, stdin=PIPE).communicate(
                self._accumulate_input()
            )


if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option('-i', '--command-interval', dest="command_interval",
                      action="store", type="float", default=1.0)
    parser.add_option('-c', '--command', dest="command", action="store",
                      default='cat')
    options, _ = parser.parse_args()
    IntervallicCommandRunner(options.command,
                             options.command_interval).loop_indefinitely()
