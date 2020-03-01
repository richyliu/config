alias ll="ls -lh"
alias pyserver="python3 -m http.server"
alias phpserver="php -S 127.0.0.1:8000 -t ."
alias compress="mogrify -resize 1200x1200 -strip -quality 80% *.jpg"
alias compressmedium="mogrify -resize 1680x1680 -strip -quality 80% *.jpg"
alias compressmin="mogrify -resize 2016x2016 -strip -quality 80% *.jpg"
alias compressfast='for i in *; do epeg -m 2016 -q 85 $i _$i; mv -f _$i $i; echo $i; done; compress'
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias .+='cd ../$(printf "%02d" $((${PWD##*/} + 1)))'
alias lltr="ll -tr"
alias gp="git pull"
alias gac="git add -A && git commit -m"
alias gs="git status"
alias gd="git diff"
alias ga="git add -A ."
alias llsr="ll -Sr"
alias duhd="du -hd1 | sort -h"
alias ta="tmux attach-session"
alias td="tmux detach-client -P"
alias epeg='/home/richard/richard/epeg-0.9.2/src/bin/epeg'

alias moshremote="mosh --ssh='ssh -p 5731' richard@73.252.205.89"

VWH="/var/www/html"
VWHM="/var/www/html/images/metart"

# make history files larger
HISTFILESIZE=10000
HISTSIZE=10000

# ignore starting with space and duplicate commands
HISTCONTROL=ignoreboth

# make it harder to accidentally overwrite stuff
set -o noclobber  # No overwrite file with > instead use >!
alias cp='cp -i'  # Prompt if overwriting
alias mv='mv -i'  # Prompt if overwriting
alias rm='rm -i'  # Prompt if removing 3+ files or recusively

bold=$(tput bold)
normal=$(tput sgr0)

function swap() {
  local TMPFILE=tmp.$$
  mv "$1" $TMPFILE
  mv "$2" "$1"
  mv $TMPFILE "$2"
}

# Allows for cd without args to go to ~/Downloads/
function cd () {
  if [ $# = 0 ]; then
    builtin cd ~/Downloads/
  else
    builtin cd "$@"
  fi
}

# Grep displays line numbers when not in a pipeline
function grep() { 
  if [[ -t 1 ]]; then 
    command grep -n "$@"
  else 
    command grep "$@"
  fi
}

export CDPATH=.:~/richard/
export EDITOR=vim

# allows ctrl-s to be used
stty -ixon

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
