#!/usr/bin/env python
import argparse
import os
import sys


class BrightnessManager(object):

    @classmethod
    def find_brightness(cls):
        return cls.from_path(
            os.path.join("/sys/class/backlight", os.listdir("/sys/class/backlight")[0])
        )

    @classmethod
    def from_path(cls, path):
        return cls(
            set_brightness_filepath=os.path.join(path, "brightness"),
            actual_brightness_filepath=os.path.join(path, "actual_brightness"),
            max_brightness_filepath=os.path.join(path, "max_brightness"),
        )

    def __init__(
            self, set_brightness_filepath, max_brightness_filepath, actual_brightness_filepath
    ):
        self.set_brightness_filepath = set_brightness_filepath
        self.max_brightness_filepath = max_brightness_filepath
        self.actual_brightness_filepath = actual_brightness_filepath

    @property
    def current_brightness(self):
        with open(self.actual_brightness_filepath) as fd:
            return int(fd.read())

    @property
    def max_brightness(self):
        with open(self.max_brightness_filepath) as fd:
            return int(fd.read())

    @current_brightness.setter
    def current_brightness(self, brightness):
        with open(self.set_brightness_filepath, 'w') as fd:
            fd.write(str(brightness))

    def increment_by_proportion(self, proportion):
        new_brightness = self.current_brightness + int(self.max_brightness * proportion)
        new_brightness = min(new_brightness, self.max_brightness)
        self.current_brightness = new_brightness

    @property
    def current_proportion(self):
        return float(self.current_brightness) / self.max_brightness


def build_parser():
    parser = argparse.ArgumentParser(
        description='Interact with macbook brightness',
    )
    parser.add_argument(
        "--change", "-c",
        help="Change volume by the given percentage",
        default=0
    )
    parser.add_argument(
        "--print", "-p", dest="do_print",
        action='store_true',
        default=False
    )
    return parser


if __name__ == '__main__':
    args = build_parser().parse_args()
    BrightnessManager.find_brightness().increment_by_proportion(float(args.change) / 100)
    if args.do_print:
        print(int(IntelBrightnessManager.current_proportion * 100))
