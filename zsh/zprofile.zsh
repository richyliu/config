# Should be linked to ~/.zprofile

# Add personal scripts to path
export PATH="$PATH:$HOME/vimrc/bin"

# Add cargo bin to path
export PATH="$HOME/.cargo/bin:$PATH"

# GPG password popup config
export GPG_TTY=`tty`

# Use neovim as the default editor
export EDITOR=nvim
