#!/usr/bin/env bash

# Set up minimal qmk installation for compiling keyboard firmware

KEYBOARD_DIR="~/config/keyboard"

KEYMAP_DIR="keyboards/ergodox_ez/keymaps/richyliu"

set -ex

git clone --recurse-submodules --depth=1 https://github.com/qmk/qmk_firmware
cd qmk_firmware

# these are big folders we don't need, can remove them to save space
rm -rf \
  lib/chibios/os/common/ext/ST \
  lib/chibios-contrib/ext/mcux-sdk \
  .git

# copy over keymap (can't use symlink due to docker mount)
mkdir "$KEYMAP_DIR"
cp "$KEYBOARD_DIR"/keymap.c \
  "$KEYBOARD_DIR"/rules.mk \
  "$KEYBOARD_DIR"/config.h \
  "$KEYMAP_DIR"

# remove any checks for docker-machine (since it's not necessary when compiling)
sed -i '' -e 's/^if .*! docker-machine.* then$/if false; then/' util/docker_cmd.sh

echo "firmware ready to be flashed with qmk toolbox"
