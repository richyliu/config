export PATH=/Users/richard/.bin:$PATH

setopt extended_glob

# alias l="ls -G"
# alias ll="ls -lh"
alias pyserver="python3 -m http.server"
# alias phpserver="php -S 127.0.0.1:8000 -t ."
alias compress="mogrify -resize 1200x1200 -strip -quality 80% *.jpg"
alias compresswait='for pic in *.jpg; do mogrify -resize 1200x1200 -strip -quality 80% "$pic"; sleep 1; done'
alias compressmedium="mogrify -resize 1680x1680 -strip -quality 80% *.jpg"
alias compressmin="mogrify -resize 2016x2016 -strip -quality 80% *.jpg"
alias compressfast='for i in *; do epeg -m 2016 -q 85 $i _$i; mv -f _$i $i; echo $i; done; compress'
alias compressquality="mogrify -strip -quality 80% *.jpg"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias .+='cd ../$(printf "%02d" $((${PWD##*/} + 1)))'
alias lltr="ll -tr"
alias llsr="ll -Sr"
alias duhd="du -hd1 | sort -h"
alias ta="tmux attach-session"
alias td="tmux detach-client -P"
alias moshremote="mosh --ssh='ssh -p 5731' richard@73.252.205.89"
alias vim='nvim'
alias vimnormal='\vim'

alias gcsm='git commit -S -m'
alias gdt='git difftool'
alias gll="git log -10 --reverse --pretty='%h - %d %s (%cr) <%an>' | cat"


# display tabs with width of 4
LESS="-Rx4";export LESS
