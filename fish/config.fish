if status is-interactive
    abbr -a _ less
    abbr -a duhd du -hd1
    abbr -a l ls -lah
    abbr -a ll ls -lh
    abbr -a lltr ls -lthr

    abbr -a ga git add
    abbr -a gaa git add -A
    abbr -a gau git add -u
    abbr -a gba git branch -a
    abbr -a gc git commit
    abbr -a gcl git clone
    abbr -a gcmsg git commit -m
    abbr -a gcn git commit -n
    abbr -a gd git diff
    abbr -a gds git diff --cached
    abbr -a gfo git fetch origin
    abbr -a gl git pull
    abbr -a gp git push
    abbr -a grs git restore
    abbr -a grst git restore --staged
    abbr -a gsh git show
    abbr -a gss git status --short
    abbr -a gst git status
    abbr -a gsw git switch
    abbr -a gswc git switch -c

    abbr -a glg 'git log --stat'
    abbr -a glgp 'git log --stat --patch'
    abbr -a glod "git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset'"
    abbr -a glods "git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset' --date=short"
    abbr -a glol "git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset'"
    abbr -a glola "git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset' --all"

    function multicd
        echo cd (string repeat -n (math (string length -- $argv[1]) - 1) ../)
    end
    abbr --add dotdot --regex '^\.\.+$' --function multicd

    if command -q fzf
        fzf --fish | source
    end
end
