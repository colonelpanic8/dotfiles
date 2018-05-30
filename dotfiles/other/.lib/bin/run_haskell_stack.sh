#!/usr/bin/env bash

cd "$( dirname "${BASH_SOURCE[0]}" )"
echo "$@"
stack build
stack runghc "$@"
