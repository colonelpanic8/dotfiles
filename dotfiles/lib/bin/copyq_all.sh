#!/usr/bin/env bash

SOURCE_DIR="$(dirname ${BASH_SOURCE[0]})"
copyq eval "$(cat $SOURCE_DIR/copyq_all.js)"
