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
  sudo apt-get update
  sudo apt-get install -y \
    git \
    htop \
    ripgrep \
    tmux \
    fzf \
    mosh
}

function install_dotfiles() {
  echo "Installing dotfiles..."
  SOURCE_STR="source ~/config/bashrc_linux"
  if ! grep -q "$SOURCE_STR" ~/.bashrc; then
    echo "$SOURCE_STR" >> ~/.bashrc
  fi
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

if [ "$1" == "--cleanup" ]; then
  cleanup
  exit 0
fi

echo "<<< Personal environment setup script >>>"

echo -n "Would you like to install packages using apt-get? [y/N] "
read -r response
if [[ "$response" =~ ^([yY][eE][sS]|[yY])$ ]]; then
  install_with_apt
fi

clone_repo

install_dotfiles

~/config/zsh/fzf/install

echo "Done."
