# Should be linked to ~/.zprofile

# enable zmv
autoload zmv

# Add personal scripts to path
export PATH="$PATH:$HOME/vimrc/bin"

# Add cargo bin to path
export PATH="$HOME/.cargo/bin:$PATH"

# GPG password popup config
export GPG_TTY=`tty`

# Use neovim as the default editor
export EDITOR=nvim

# Setting PATH for Python 3.7
# The original version is saved in .zprofile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.7/bin:${PATH}"
export PATH

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/richard/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/richard/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/richard/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/richard/google-cloud-sdk/completion.zsh.inc'; fi

export GOOGLE_APPLICATION_CREDENTIALS=/Users/richard/gregg-shorthand-07d924baf9f4.json

# JDK path
export PATH="/Users/richard/code/jdk-11.0.10+9/Contents/Home/bin:$PATH"

export HOMEBREW_NO_AUTO_UPDATE=1
