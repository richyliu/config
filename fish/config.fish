if status is-interactive
    abbr -a _ less
    abbr -a duhd 'du -hd1 | sort -h'
    abbr -a l ls -lah
    abbr -a ll ls -lh
    abbr -a lltr ls -lthr

    abbr -a codex codex --profile common

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
    abbr -a grb git rebase
    abbr -a grs git restore
    abbr -a grst git restore --staged
    abbr -a grv git remote -v
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

    function jqless --description 'Interactively view colorized JSON with jq and less'
        if test (count $argv) -gt 0
            jq -C . $argv | less -R
        else
            # Fallback for piped input (e.g., curl ... | jqless)
            jq -C . | less -R
        end
    end

    # option-backspace delete to word deliminated by symbols
    bind \e\x7f backward-kill-word

    if command -q fzf
        fzf --fish | source
    end
end

if test (uname) = Linux
    set -gx SSH_AUTH_SOCK "$HOME/.ssh/agent.sock"

    ssh-add -l >/dev/null 2>&1
    if test $status -eq 2
        # A pod restart can leave the socket behind after the agent exits.
        rm -f -- "$SSH_AUTH_SOCK"
        and eval (ssh-agent -c -a "$SSH_AUTH_SOCK")
    end
end


# Rename the original prompt function so we can still use it
functions -c fish_prompt original_fish_prompt

function fish_prompt
    set -l nix_shell_info ""
    if set -q IN_NIX_SHELL; or test "$SHLVL" -gt 1
        set nix_shell_info "<nix-shell> "
    end

    # PREPEND: Put what you want BEFORE the prompt here
    echo -n "$nix_shell_info"

    # Call the original prompt
    original_fish_prompt

    # APPEND: Put what you want AFTER the prompt here
    # (Note: Most prompts end with a newline, so appending here
    # might place text on the next line unless edited carefully)
end
