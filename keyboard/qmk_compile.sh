#!/usr/bin/env bash

QMK_DIR="$HOME/code/qmk_firmware"
KEYBOARD_DIR="$HOME/config/keyboard"
KEYMAP_DIR="keyboards/ergodox_ez/keymaps/richyliu"

set -ex

cd "$QMK_DIR"

# copy over keymap (can't use symlink due to docker mount)
mkdir -p "$KEYMAP_DIR"
cp "$KEYBOARD_DIR"/keymap.c \
  "$KEYBOARD_DIR"/rules.mk \
  "$KEYBOARD_DIR"/config.h \
  "$KEYMAP_DIR" \
    || true # ignore if files are the same

# build
./util/docker_build.sh ergodox_ez:richyliu
