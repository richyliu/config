#!/usr/bin/env bash

# bash <(curl -sL https://raw.githubusercontent.com/richyliu/config/refs/heads/master/init_linux.sh)

# This script sets up my personal environment and scripts on a new Linux
# (Debian-based) machine. It does not require sudo privileges, but if
# available, will attempt to install some packages using apt-get.

# Exit on error
set -e

cd ~

function clone_repo() {
  # check if repository already exists
  if [ -d "config" ]; then
    echo "Repository already exists. Skipping cloning."
    return
  fi

  # check if git is installed
  if ! command -v git &> /dev/null; then
    echo "Git is not installed. Please install git and try again."
    exit 1
  fi

  echo "Cloning repository..."
  git clone --recursive https://github.com/richyliu/config.git
}

function install_with_apt() {
  echo "Installing packages..."
  SUDO_PREFIX=""
  if [ "$(id -u)" -ne 0 ]; then
    SUDO_PREFIX="sudo"
  fi
  # update
  $SUDO_PREFIX apt-get update
  $SUDO_PREFIX apt-get install -y \
    git \
    htop \
    ripgrep \
    tmux \
    mosh
}

function install_dotfiles() {
  echo "Installing dotfiles..."

  SOURCE_STR="source ~/config/bashrc_linux"
  if ! grep -q "$SOURCE_STR" ~/.bashrc; then
    echo "$SOURCE_STR" >> ~/.bashrc
  fi

  if [ -f ~/.bash_profile ]; then
    echo "Backing up existing .bash_profile to .bash_profile.bak"
    mv ~/.bash_profile ~/.bash_profile.bak
  fi
  ln -s ~/config/.bash_profile ~/.bash_profile

  if [ -f ~/.bash_aliases ]; then
    echo "Backing up existing .bash_aliases to .bash_aliases.bak"
    mv ~/.bash_aliases ~/.bash_aliases.bak
  fi
  ln -s ~/config/bash_aliases ~/.bash_aliases
  if [ -f ~/.tmux.conf ]; then
    echo "Backing up existing .tmux.conf to .tmux.conf.bak"
    mv ~/.tmux.conf ~/.tmux.conf.bak
  fi
  ln -s ~/config/tmux.conf ~/.tmux.conf
  if [ -f ~/.vimrc ]; then
    echo "Backing up existing .vimrc to .vimrc.bak"
    mv ~/.vimrc ~/.vimrc.bak
  fi
  ln -s ~/config/vim/basic.vim ~/.vimrc
}

function cleanup(){
  echo "Cleaning up..."

  rm -f ~/.bash_aliases
  rm -f ~/.tmux.conf
  rm -f ~/.vimrc
  rm -rf ~/config

  echo "Remove the following line from ~/.bashrc:"
  echo "$SOURCE_STR"

  echo "Done."
}


NON_INTERACTIVE=${NON_INTERACTIVE:-false}
FORCE_INSTALL=${FORCE_INSTALL:-false}

# allow for non-interactive mode with the --non-interactive flag
if [ "$1" == "--non-interactive" ]; then
  NON_INTERACTIVE=true
fi

# print a message for non-interactive mode
if [ "$NON_INTERACTIVE" = true ]; then
  echo "Running in non-interactive mode."
fi

if [ "$1" == "--cleanup" ]; then
  cleanup
  exit 0
fi

echo "<<< Personal environment setup script >>>"

if [ "$NON_INTERACTIVE" = false ]; then
  echo -n "Would you like to install packages using apt-get? [y/N] "
  read -r response
  if [[ "$response" =~ ^([yY][eE][sS]|[yY])$ ]]; then
  install_with_apt
  fi
else
  if [ "$FORCE_INSTALL" = true ]; then
    echo "Forcing package installation..."
    echo "You can change this behavior by setting FORCE_INSTALL=false"
    install_with_apt
  else
    echo "Skipping package installation..."
    echo "You can change this behavior by setting FORCE_INSTALL=true"
  fi
fi

clone_repo

install_dotfiles

if [ "$NON_INTERACTIVE" = false ]; then
  ~/config/zsh/fzf/install
else
  yes | ~/config/zsh/fzf/install
fi

echo "Done."
