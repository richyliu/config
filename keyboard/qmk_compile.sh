#!/usr/bin/env bash

QMK_DIR="$HOME/code/qmk_firmware"

set -ex

cd "$QMK_DIR"

# build
SKIP_FLASHING_SUPPORT=1 ./util/docker_build.sh ergodox_ez:richyliu
