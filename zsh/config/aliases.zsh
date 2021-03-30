# Should be linked to $ZSH_CUSTOM/aliases.zsh
# Usually ~/.oh-my-zsh/custom/aliases.zsh

alias vim=nvim
alias vimb="nvim -u ~/vimrc/vim/basic.vim"
alias vimbs="nvim -u ~/vimrc/vim/basic.vim +syntax\ off"
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
