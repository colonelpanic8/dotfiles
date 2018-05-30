import logging

from coloredlogs import ColoredStreamHandler


def enable_logger(log_name, level=logging.DEBUG):
    log = logging.getLogger(log_name)
    handler = ColoredStreamHandler(severity_to_style={'WARNING': dict(color='red')})
    handler.setLevel(level)
    log.setLevel(level)
    log.addHandler(handler)
