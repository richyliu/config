# Should be linked to ~/.zprofile

# enable zmv
autoload zmv

# Add personal scripts to path
export PATH="$PATH:$HOME/config/bin"

# Add cargo bin to path
export PATH="$HOME/.cargo/bin:$PATH"

export PATH=/opt/homebrew/bin:$PATH

export PATH="$HOME/.local/bin:$PATH"

# GPG password popup config
export GPG_TTY=`tty`

# Use neovim as the default editor
export EDITOR=nvim

export HOMEBREW_NO_AUTO_UPDATE=1

# Load homebrew autocompletion
FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH

# Add python bin to path
export PATH="$PATH:$HOME/Library/Python/3.9/bin"

# Add ruby to path
export PATH="$PATH:$HOME/.gem/ruby/2.6.0/bin"

# show line numbers by default in less
export LESS="-R -M"
# go to end of file and back to beginning so that less shows percentage info
export MANPAGER="less +Gg"
