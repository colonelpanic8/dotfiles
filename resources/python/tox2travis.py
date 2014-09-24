#!/usr/bin/env python
from tox._config import parseconfig
from yaml import dump


class TravisFromTox(object):

    def __init__(self, tox_config):
        self._tox_config = tox_config

    def build_travis_dict(self):
        return {
            'language': 'python',
            'install': ['pip install -e hg+https://ivanmalison@bitbucket.org/hpk42/tox#egg=tox'],
            'script': 'tox',
            'env': self._get_environment_variables()
        }

    def _get_environment_variables(self):
        return ['TOXENV={0}'.format(env) for env in self._tox_config.envlist]

    def build_travis_yaml(self):
        return dump(self.build_travis_dict(), default_flow_style=False)


if __name__ == '__main__':
    print TravisFromTox(parseconfig()).build_travis_yaml()
