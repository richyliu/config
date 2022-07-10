# Grievances

- alt-backspace in vterm does not work
- pasting text in vterm over ssh in a fgets prompt hangs emacs (it keeps sending `^[D`)?
- cannot complete filepath over tramp?
- "Node 12+ is required but found 0" when editing over tramp
- tramp auto save over network delay is slow
    - autosave locally?
- add shortcuts to open project's ibuffer/vterm
- paste without indent (`]p`)
- git gutter does not work over TRAMP
- everything over TRAMP is slow
- make TRAMP commands async
- **emacs freezes when sending ^[[D over ssh**

## Fixed
- vterm closes immediately when in a TRAMP project
    - set `vterm-tramp-shells` to include `/bin/bash`
- projectile-project-vterm over remote ssh does not select correct shell
    - set `vterm-tramp-shells` manually
- can't jump back in apropos
    - used `pop-global-mark` key: `C-x C-SPC`
- add shortcut to kill-this-buffer
    - already exists (`SPC-b-d`)
