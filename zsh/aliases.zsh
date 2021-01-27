# Should be linked to $ZSH_CUSTOM/aliases.zsh
# Usually ~/.oh-my-zsh/custom/aliases.zsh

alias vim_real=/usr/bin/vim
alias vim="nvim -w ~/vimrc/vim_key_logs"
alias ag="ag -f --color-line-number 1\;35"
alias agl="ag -f --color-line-number 1\;35 --pager less"

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

alias pjl="/Users/richard/plojo/target/release/lookup"
