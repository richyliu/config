# Grievances

- **vterm windows killed when switching to other tab (more than 1 vterm window)**
    - look into `SPC o t` and `SPC o T`
- paste without indent (`]p`)
    - https://github.com/emacs-evil/evil/issues/464
    - https://github.com/doomemacs/doomemacs/blob/develop/modules/editor/evil/config.el#L413-L460
    - https://github.com/tpope/vim-unimpaired/blob/efdc6475f7ea789346716dabf9900ac04ee8604a/doc/unimpaired.txt#L104-L109
- git gutter does not work over TRAMP
- disable tab for yas-expand
- add shortcut to execute the current file
- add shortcut to fuzzy find file from current directory

- tramp auto save over network delay is slow
    - autosave locally?
- everything over TRAMP is slow
- make TRAMP commands async
- emacs freezes when sending ^[[D over ssh
    - temp fix: `c-g`
- "Node 12+ is required but found 0" when editing over tramp?
- cannot complete filepath over tramp?

- minibuffer history
    - real edit mode (instead of just ctrl-f)

## Fixed
- vterm closes immediately when in a TRAMP project
    - set `vterm-tramp-shells` to include `/bin/bash`
- projectile-project-vterm over remote ssh does not select correct shell
    - set `vterm-tramp-shells` manually
- can't jump back in apropos
    - used `pop-global-mark` key: `C-x C-SPC`
- add shortcut to kill-this-buffer
    - already exists (`SPC-b-d`)
- anaconda-mode over tramp
- disable/fix irony mode over ssh
- man pages very slow
    - one solution: `brew install man-db` and using `mandb` to cache database
        - found [here](https://github.com/abo-abo/swiper/issues/2836#issuecomment-831292443)
        - doesn't work, as it doesn't include all the man pages
    - another solution: use woman (doom emacs has a `+default/man-or-woman`)
        - doesn't work, as woman can't display Mac's custom man format
    - solved by using advice-add to manually add caching to Man-completion-table
- (centaur) tabs not grouping by projectile project
    - created custom tab group function
- add shortcuts to open project's ibuffer/vterm
    - added `SPC p v`
- alt-backspace in vterm does not work
    - fixed by defining key map in vterm insert mode
- add shortcut to send `C-x C-e` while in vterm
    - added `C-x C-x`
- use different prefix (not `C-c`) for vterm
    - switched prefix to `C-x C-a`
