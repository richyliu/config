# Should be linked to $ZSH_CUSTOM/aliases.zsh
# Usually ~/.oh-my-zsh/custom/aliases.zsh

alias vim=nvim
alias vimb="nvim -u ~/vimrc/vim/basic.vim"
alias ag="ag -f --color-line-number 1\;35"
alias agq="ag -fQ --color-line-number 1\;35"
alias agl="ag -f --color-line-number 1\;35 --pager less"
alias aglq="ag -fQ --color-line-number 1\;35 --pager less"
alias tree="tree -I node_modules"

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."

alias lltr="ll -tr"
alias llsr="ll -Sr"

alias duhd="du -hd1 | sort -h"

# Oh-my-zsh git overrides
alias gcsm='git commit -S -m'
alias grs='git restore --staged'
alias gai="git add --interactive"

alias crc='cargo check'
alias crct='cargo check --tests'
alias crt='cargo test'
alias crtq='cargo test --quiet'

alias pjl="/Users/richard/plojo/target/release/lookup"

alias wcl="wc -l"

alias .+='cd ../$(printf "%02d" $(( ${PWD##*/} + 1 )))'

function swap() {
  local TMPFILE=tmp.$$
  mv "$1" $TMPFILE && mv "$2" "$1" && mv $TMPFILE "$2"
}

function pics_compress() {
  COUNT=1
  echo "Compressing all jpgs in this folder with 1 sec delay between each..."
  for img in *.jpg; do
    mogrify -resize 1200x1200 -quality 80% -strip "$img"

    if (( COUNT % 10 == 0 )); then
      echo -n " $COUNT "
    else
      echo -n "."
    fi
    (( COUNT++ ))

    sleep 1
  done
  echo ""
  echo "Done"

}

function compress() {
  if [[ $1 == "--help" ]]; then
    echo "Usage: compress [FOLDERS...]"
    echo ""
    echo "Compresses all .jpg images in [FOLDERS...]. If no folders are provided,"
    echo "compress all images in the current directory"
    return
  fi

  # Compress all jpgs if no folders are specified
  if [[ $# -eq 0 ]]; then
    pics_compress
  else
    echo "Compressing all jpgs in: $*"
    for folder in "$@"; do
      if [[ -d "$folder" ]]; then
        (
        cd "$folder" || exit
        echo "In folder: $folder"
        pics_compress
        )
      else
        echo "WARNING: folder $folder does not exist"
      fi
    done
  fi
}

