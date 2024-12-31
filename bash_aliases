alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."

alias l="ls -la"
alias ll="ls -lh"
alias lltr="ll -tr"
alias llsr="ll -Sr"

alias duhd="du -hd1 | sort -h"

alias gd='git diff'
alias gds='git diff --staged'
alias glol='git log --graph --pretty='\''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset'\'
alias glola='git log --graph --pretty='\''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset'\'' --all'
alias glods='git log --graph --pretty='\''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset'\'' --date=short'
alias glod='git log --graph --pretty='\''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset'\'
alias glg='git log --stat'
alias glgg='git log --graph'
alias glgp='git log --stat -p'
alias gp='git push'
alias gl='git pull'
alias gc='git commit -v'
alias gcmsg='git commit -m'
alias gai="git add --interactive"
alias ga='git add'
alias gss='git status -s'
alias gst='git status'
alias gaa='git add --all'
alias grv='git remote -v'
alias gba='git branch -a'
alias gsw='git switch'
alias gco='git checkout'
alias grs='git restore'
alias grst='git restore --staged'
alias gsta='git stash push'
alias gstp='git stash pop'

# to make piping to less easier to type ("|_")
alias _="less"
