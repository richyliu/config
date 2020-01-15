#
# ~/.bashrc
#

[[ $- != *i* ]] && return

colors() {
  local fgc bgc vals seq0

  printf "Color escapes are %s\n" '\e[${value};...;${value}m'
  printf "Values 30..37 are \e[33mforeground colors\e[m\n"
  printf "Values 40..47 are \e[43mbackground colors\e[m\n"
  printf "Value  1 gives a  \e[1mbold-faced look\e[m\n\n"

  # foreground colors
  for fgc in {30..37}; do
    # background colors
    for bgc in {40..47}; do
      fgc=${fgc#37} # white
      bgc=${bgc#40} # black

      vals="${fgc:+$fgc;}${bgc}"
      vals=${vals%%;}

      seq0="${vals:+\e[${vals}m}"
      printf "  %-9s" "${seq0:-(default)}"
      printf " ${seq0}TEXT\e[m"
      printf " \e[${vals:+${vals+$vals;}}1mBOLD\e[m"
    done
    echo; echo
  done
}

[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion

# Change the window title of X terminals
case ${TERM} in
  xterm*|rxvt*|Eterm*|aterm|kterm|gnome*|interix|konsole*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\007"'
    ;;
  screen*)
    PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\033\\"'
    ;;
esac

use_color=true

# Set colorful PS1 only on colorful terminals.
# dircolors --print-database uses its own built-in database
# instead of using /etc/DIR_COLORS.  Try to use the external file
# first to take advantage of user additions.  Use internal bash
# globbing instead of external grep binary.
safe_term=${TERM//[^[:alnum:]]/?}   # sanitize TERM
match_lhs=""
[[ -f ~/.dir_colors   ]] && match_lhs="${match_lhs}$(<~/.dir_colors)"
[[ -f /etc/DIR_COLORS ]] && match_lhs="${match_lhs}$(</etc/DIR_COLORS)"
[[ -z ${match_lhs}    ]] \
  && type -P dircolors >/dev/null \
  && match_lhs=$(dircolors --print-database)
[[ $'\n'${match_lhs} == *$'\n'"TERM "${safe_term}* ]] && use_color=true

if ${use_color} ; then
  # Enable colors for ls, etc.  Prefer ~/.dir_colors #64489
  if type -P dircolors >/dev/null ; then
    if [[ -f ~/.dir_colors ]] ; then
      eval $(dircolors -b ~/.dir_colors)
    elif [[ -f /etc/DIR_COLORS ]] ; then
      eval $(dircolors -b /etc/DIR_COLORS)
    fi
  fi

  if [[ ${EUID} == 0 ]] ; then
    PS1='\[\033[01;31m\][\h\[\033[01;36m\] \W\[\033[01;31m\]]\$\[\033[00m\] '
  else
    PS1='\[\033[01;32m\][\u@\h\[\033[01;37m\] \W\[\033[01;32m\]]\$\[\033[00m\] '
  fi

  alias ls='ls --color=auto'
  alias grep='grep --colour=auto'
  alias egrep='egrep --colour=auto'
  alias fgrep='fgrep --colour=auto'
else
  if [[ ${EUID} == 0 ]] ; then
    # show root@ when we don't have colors
    PS1='\u@\h \W \$ '
  else
    PS1='\u@\h \w \$ '
  fi
fi

unset use_color safe_term match_lhs sh

xhost +local:root > /dev/null 2>&1

complete -cf sudo

# Bash won't get SIGWINCH if another process is in the foreground.
# Enable checkwinsize so that bash will check the terminal size when
# it regains control.  #65623
# http://cnswww.cns.cwru.edu/~chet/bash/FAQ (E11)
shopt -s checkwinsize

shopt -s expand_aliases

# export QT_SELECT=4

# Enable history appending instead of overwriting.  #139609
shopt -s histappend

#
# # ex - archive extractor
# # usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1     ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

# better yaourt colors
export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"


alias ll="ls -lh --color=always"
alias utorrent="utserver -settingspath /opt/utorrent-server-alpha-v3_3/"
alias pyserver="python3 -m http.server"
alias phpserver="php -S 127.0.0.1:8000 -t ."
alias compress="mogrify -resize 1200x1200 -strip -quality 80% *.jpg"
alias compressmedium="mogrify -resize 1680x1680 -strip -quality 80% *.jpg"
alias compressmin="mogrify -resize 2016x2016 -strip -quality 80% *.jpg"
alias compressfast='for i in *; do epeg -m 2016 -q 85 $i _$i; mv _$i $i; echo $i; done; compress'
alias nvi="~/nvim.appimage"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias .+='cd ../$(printf "%02d" $((${PWD##*/} + 1)))'
alias cds='cd ~/Downloads/stuff/'
alias pi="ssh pi@10.0.1.11"
alias lltr="ll -tr"
alias beep="play -q /home/richard/media/beep.ogg"
alias gp="git pull"
alias gac="git add -A && git commit -S -m"
alias gs="git status"
alias gd="git diff"
alias ga="git add -A ."
alias llsr="ll -Sr"
alias xopen="xdg-open"
alias duhd="du -hd1 | sort -h"
alias ta="tmux attach-session"
alias td="tmux detach-client -P"
alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'
alias epeg='/home/richard/richard/epeg-0.9.2/src/bin/epeg'
alias renumber='perl-rename '"'"'s/(.+)/_$1/'"'"' *; let i=0; for f in *; do mv "$f" $(printf "%03d.jpg" $i); let i++; done'

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
alias rm='rm -I'  # Prompt if removing 3+ files or recusively

# Compress video length and width by scale factor and crf (constant rate factor)
function ffmpeg_compress() {
  if [ $# -eq 0 ]; then
    echo "Usage: ffmpeg_compress SRC_VIDEO SCALE_FACTOR CRF OUTPUT_VIDEO"
    echo "    Recommended SCALE_FACTOR is 3 and CRF is 28"
    return 1
  fi

  echo "Compressing video '$1' by a scale factor of $2 and a crf of $3 to '$4'"
  echo "Recommended scale factor is 3 and crf is 28"
  read -p "Would you like output to be silenced? [y/N]: " silence
  case $silence in
    y|Y) ffmpeg -i "$1" -strict -2 -vf "scale=iw/$2:ih/$2" -vcodec libx265 \
      -crf $3 -loglevel 24 "$4" ;;
    *) ffmpeg -i "$1" -strict -2 -vf "scale=iw/$2:ih/$2" -vcodec libx265 \
      -crf $3 "$4" ;;
  esac
}

# Creates a fullscreen slideshow
function fullscreen() {
  slideshow $@ -F
}

# allows a command to be run in each folder
function for_command() {
  echo "Running '$1' in the folders: ${@:2}"
  read -p "Press ENTER to continue: "

  for i in "${@:2}"; do
    if [ -d "$i" ]; then
      cd "$i";
      eval "$1";
      cd ..;
    else
      echo "Error: $i does not exist"
    fi
  done
}

export CDPATH=.:~/richard/
export TCELL_TRUECOLOR=disable
export TERM="screen-256color"
export EDITOR=vim

export PATH="$PATH:/home/richard/.gem/ruby/2.6.0/bin"

export PATH="$PATH:/home/richard/bin"

function swap() {
  local TMPFILE=tmp.$$
  mv "$1" $TMPFILE
  mv "$2" "$1"
  mv $TMPFILE "$2"
}
# Allows for cd without args to go to ~/Downloads/
cd () {
  if [ $# = 0 ]; then
    builtin cd ~/Downloads/
  else
    builtin cd "$@"
  fi
}

# count number of words in an HTML file (removes HTML tags)
function countwords() {
  if [ $# -eq 0 ]; then
    echo "Usage: countwords HTML_FILE [FIRST_LINES]"
    echo "    Counts words by stripping html tags from the file."
    echo "    The first FIRST_LINES lines of the file will be displayed. Default is 15"
    return 1
  fi

  filename=$1
  first_lines=15
  sed 's/<[^>]\+>/ /g' $filename | wc -w
  if [ $# -eq 2 ]; then
    first_lines=$2
  fi
  if [[ $# > 0 ]]; then
    sed 's/<[^>]\+>/ /g' $filename | fold -w 80 -s | head -n $first_lines
  fi
}

# allow GPG to work correctly
GPG_TTY=$(tty)
export GPG_TTY

# allows ctrl-s to be used
stty -ixon

# Expand directories in a variable
shopt -s direxpand

# powerline fonts
. /usr/share/bash/powerline.sh

# nvm node version manager
source /usr/share/nvm/init-nvm.sh

# automatically attach tmux session on ssh
if [[ -n "$PS1"  ]] && [[ -z "$TMUX"  ]] && [[ -n "$SSH_CONNECTION"  ]]; then
  tmux attach-session
fi
