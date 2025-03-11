#!/usr/bin/env bash

QMK_DIR="$HOME/code/qmk_firmware"

set -ex

cd "$QMK_DIR"

# build
./util/docker_build.sh ergodox_ez:richyliu
