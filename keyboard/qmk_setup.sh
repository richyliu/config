#!/usr/bin/env bash

# Set up minimal qmk installation for compiling keyboard firmware

QMK_DIR="$HOME/code/qmk_firmware"
KEYBOARD_DIR="$HOME/config/keyboard"
KEYMAP_DIR="keyboards/ergodox_ez/keymaps/richyliu"

set -ex

git clone --recurse-submodules --depth=1 https://github.com/qmk/qmk_firmware "$QMK_DIR"
cd "$QMK_DIR"

# these are big folders we don't need, can remove them to save space
rm -rf \
  lib/chibios/os/common/ext/ST \
  lib/chibios-contrib/ext/mcux-sdk \
  .git

# copy over keymap (can't use symlink due to docker mount)
mkdir -p "$KEYMAP_DIR"
ln "$KEYBOARD_DIR"/keymap.c \
  "$KEYBOARD_DIR"/rules.mk \
  "$KEYBOARD_DIR"/config.h \
  "$KEYMAP_DIR"

# remove any checks for docker-machine (since it's not necessary when compiling)
sed -i '' -e 's/^if .*! docker-machine.* then$/if false; then/' util/docker_cmd.sh

echo "QMK is set up."
