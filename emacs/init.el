;; (setq use-package-compute-statistics t)

(tool-bar-mode -1)             ; Hide the outdated icons
(scroll-bar-mode -1)           ; Hide the always-visible scrollbar
(setq inhibit-splash-screen t) ; Remove the "Welcome to GNU Emacs" splash screen
(setq use-file-dialog nil)     ; Ask for textual confirmation instead of GUI


; straight.el setup
(defvar bootstrap-version)
(let ((bootstrap-file
    (expand-file-name
      "straight/repos/straight.el/bootstrap.el"
      (or (bound-and-true-p straight-base-dir)
        user-emacs-directory)))
    (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
       'silent 'inhibit-cookies)
    (goto-char (point-max))
    (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

; install use-package
(straight-use-package 'use-package)

; make use-package use straight.el
(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)

(setq use-package-always-ensure t)


(use-package emacs
  :init

  ; disable startup message
  (setq initial-scratch-message nil)
  (defun display-startup-echo-area-message ()
    (message ""))

  ; shorter confirmation y/n
  (defalias 'yes-or-no-p 'y-or-n-p)

  ; use utf-8
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  ; use spaces
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)

  ; fix macos keybindings
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'super)
    (setq mac-option-modifier 'meta)
    (setq mac-control-modifier 'control))

  (setq backup-directory-alist `(("." . ,(expand-file-name "saves/backups" user-emacs-directory)))
        auto-save-file-name-transforms `((".*" ,(expand-file-name "saves/auto-saves" user-emacs-directory) t))
        vc-follow-symlinks nil
        compilation-window-height 20)

  ; enable relative line numbers in prog-mode
  (defun my/enable-line-numbers ()
    "Enable relative line numbers"
    (interactive)
    (display-line-numbers-mode)
    (setq display-line-numbers 'relative))
  (add-hook 'prog-mode-hook #'my/enable-line-numbers)

  (set-frame-font (font-spec
                   :family "iosevka term ss07"
                   :width 'expanded
                   :size 15
                   ))
  (set-face-attribute 'default nil :height 150)

  (add-to-list 'safe-local-variable-values '(eval auto-revert-mode 1))
  (add-to-list 'safe-local-variable-values '(display-line-numbers . visual))

  (setq display-line-numbers-type 'relative)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'text-mode-hook 'display-line-numbers-mode)
  )

(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one-light t)
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

(use-package general
  :demand t
  :config
  (general-evil-setup t)
  (defconst my-leader "SPC")
  (defconst my-insert-leader "M-SPC")
  (general-create-definer leader-def
                          :prefix my-leader
                          :non-normal-prefix my-insert-leader
                          :keymaps 'override
                          :states '(normal visual motion insert emacs))
  (general-override-mode) ;; https://github.com/noctuid/general.el/issues/99#issuecomment-360914335

  (defun my/kill-all ()
    "Kill all buffers in buffer-list and cd back to home"
    (interactive)
    (ignore-errors (mapc #'kill-buffer (buffer-list)))
    (cd "~/")
    (delete-other-windows))

  (defun my/default-agenda-view ()
    "Open my personal split screen agenda view"
    (interactive)
    (delete-other-windows)
    ;; disable popup for file selection in project
    (setq current-prefix-arg t)
    ;; switch to org-directory project first to avoid projectile issues
    ;; (projectile-switch-project-by-name org-directory)
    (find-file (concat org-directory "temp_agenda.org"))
    (find-file (concat org-directory "inbox.org"))
    (find-file (concat org-directory "agenda.org"))
    ;; open up org-agenda and agenda.org side by side
    (evil-window-vsplit)
    (org-agenda nil "d")
    ;; ugly hack to refresh org-agenda after inline links are rendered
    (sleep-for 0.01)
    (org-agenda-redo)
    )

  ;; doomesque hotkeys using spacebar as prefix
  (leader-def
    ;; map universal argument to SPC-u
    "u" '(universal-argument :which-key "Universal argument")
    "." #'find-file
    ";" #'pp-eval-expression

    "qq" #'kill-emacs
    "qa" #'my/kill-all

    "bi" #'ibuffer
    "bk" #'kill-current-buffer

    "he" #'view-echo-area-messages
    "hl" #'view-lossage
    "hm" #'describe-mode

    "na" #'org-agenda
    "nt" #'org-todo-list
    "nl" #'org-store-link
    "n SPC" #'my/default-agenda-view

    "tw" #'visual-line-mode
    )
  )

(use-package helpful
  :init
  (defvar read-symbol-positions-list nil)
  :config
  ;; redefine help keys to use helpful functions instead of vanilla
  ;; https://github.com/Wilfred/helpful#usage
  :general ;; global
  (leader-def
    "hf" #'helpful-callable
    "hx" #'helpful-command
    "hv" #'helpful-variable
    "hk" #'helpful-key)
  (general-define-key
   :keymaps 'helpful-mode-map
   :states 'normal
   "q" #'quit-window)
  )

(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-C-u-scroll t
        evil-want-C-u-delete t
        evil-want-minibuffer t
        evil-want-keybinding nil
        evil-want-Y-yank-to-eol t
        evil-search-module 'evil-search
        evil-undo-system 'undo-redo)
  :config
  (leader-def
    "ww" #'evil-window-next
    "wh" #'evil-window-left
    "wl" #'evil-window-right
    "wk" #'evil-window-up
    "wj" #'evil-window-down
    "wv" #'evil-window-vsplit
    "ws" #'evil-window-split
    "wd" #'evil-window-delete
    )
  (evil-mode 1)
  (evil-define-key 'normal 'help-mode-map (kbd "TAB") #'forward-button)
  )

(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init '(calendar dired calc ediff)))

(use-package vertico
  :demand t
  :init
  :config
  (general-define-key
    :keymaps 'vertico-map
    :states 'insert
    "C-j" #'vertico-next
    "C-k" #'vertico-previous
    ; "C-u" #'vertico-scroll-down
    "C-d" #'vertico-scroll-up
    ; page down and page up keys
    "<next>" #'vertico-scroll-down
    "<prior>" #'vertico-scroll-down
    "RET" #'vertico-directory-enter
    "DEL" #'vertico-directory-delete-char
    )
  (general-define-key
    :keymaps 'vertico-map
    :states 'normal
    "C-j" #'vertico-next
    "C-k" #'vertico-previous
    "C-u" #'vertico-scroll-down
    "C-d" #'vertico-scroll-up
    "<escape>" #'abort-recursive-edit
    )
  (leader-def
    "'" #'vertico-repeat)
  (require 'vertico-repeat)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (vertico-mode))

(use-package which-key
  :general
  (leader-def
    "hbf" #'which-key-show-full-keymap
    "hbk" #'which-key-show-keymap)
  )

(use-package which-key
    :init
    (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
    (which-key-mode))

(use-package consult
  :general
  (leader-def
    ;; "fr" #'consult-recent-file
    "ss" #'consult-line
    "si" #'consult-imenu
    "," #'consult-project-buffer
    "<" #'consult-buffer)
  )

(use-package recentf
  :demand t
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-saved-items 100
        recentf-exclude '("/tmp/")))

(use-package org
  :init
  (setq org-directory "/Users/richard/Documents/org/agenda/")
  (setq org-agenda-files '("inbox.org" "agenda.org" "temp_agenda.org"))

  :general
  (defun my/temp-refile ()
    (interactive)
    (org-schedule nil "+0d")
    (org-todo "TEMP")
    (let* ((temp-agenda-file (concat org-directory "temp_agenda.org"))
           (target-rfloc (list "temp_agenda.org" temp-agenda-file nil nil)))
      (org-refile nil nil target-rfloc nil)))

  (general-define-key
    :prefix my-leader
    :non-normal-prefix my-insert-leader
    :keymaps 'org-mode-map
    :states '(normal visual motion insert emacs)
    "mds" #'org-schedule
    "mdd" #'org-deadline
    "mcE" #'org-set-effort
    "mpp" #'org-priority

    "mA" #'org-archive-subtree-default
    "mh" #'org-toggle-heading
    "mi" #'org-toggle-item
    "mo" #'org-set-property
    "mt" #'org-todo
    "mx" #'org-toggle-checkbox

    ;; "msb" #'org-tree-to-indirect-buffer
    "msc" #'org-clone-subtree-with-time-shift
    "msd" #'org-cut-subtree
    "msh" #'org-promote-subtree
    "msj" #'org-move-subtree-down
    "msk" #'org-move-subtree-up
    "msl" #'org-demote-subtree
    "msn" #'org-narrow-to-subtree
    "msr" #'org-refile
    ;; "mss" #'org-sparse-tree
    "msA" #'org-archive-subtree-default
    "msN" #'widen

    "m'" #'org-edit-special
    "m," #'org-switchb
    "m." #'consult-org-heading
    "mT" #'my/temp-refile
    "mN" #'org-add-note
    "msR" #'org-fold-reveal
    )
  (general-define-key
    :states 'motion
    :keymaps 'org-mode-map
    ;; "^" #'org-beginning-of-line
    ;; "gk" #'org-backward-element
    ;; "gj" #'org-forward-element
    ;; "gh" #'org-up-element
    ;; "gl" #'org-down-element
    "s-<up>" #'org-up-element
    "s-k" #'org-insert-link
    )
  (general-define-key
    :states 'motion
    :keymaps 'org-agenda-mode-map
    "cs" #'org-agenda-schedule
    "cd" #'org-agenda-deadline
    "s-s" #'org-save-all-org-buffers
    "s-r" #'org-agenda-redo
    "r" #'org-agenda-redo
    )
  (general-define-key
    :states '(normal insert visual)
    :keymaps 'org-read-date-minibuffer-local-map
    "C-h" #'(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1)))
    "C-l" #'(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1)))
    "C-k" #'(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1)))
    "C-j" #'(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1)))
    )

  :config

  ;; BEGIN from doom emacs

  (defvar +org-habit-graph-padding 2
    "The padding added to the end of the consistency graph")
  (defvar +org-habit-min-width 30
    "Hides the consistency graph if the `org-habit-graph-column' is less than this value")
  (defvar +org-habit-graph-window-ratio 0.3
    "The ratio of the consistency graphs relative to the window width")

  (add-hook 'org-agenda-mode-hook
            (defun +org-habit-resize-graph-h ()
              "Right align and resize the consistency graphs based on
`+org-habit-graph-window-ratio'"
              (require 'org-habit)
              (let* ((total-days (float (+ org-habit-preceding-days org-habit-following-days)))
                     (preceding-days-ratio (/ org-habit-preceding-days total-days))
                     (graph-width (floor (* (window-width) +org-habit-graph-window-ratio)))
                     (preceding-days (floor (* graph-width preceding-days-ratio)))
                     (following-days (- graph-width preceding-days))
                     (graph-column (- (window-width) (+ preceding-days following-days)))
                     (graph-column-adjusted (if (> graph-column +org-habit-min-width)
                                                (- graph-column +org-habit-graph-padding)
                                              nil)))
                (setq-local org-habit-preceding-days preceding-days)
                (setq-local org-habit-following-days following-days)
                (setq-local org-habit-graph-column graph-column-adjusted))))

  (defun +org--insert-item (direction)
    (let ((context (org-element-lineage
                    (org-element-context)
                    '(table table-row headline inlinetask item plain-list)
                    t)))
      (pcase (org-element-type context)
        ;; Add a new list item (carrying over checkboxes if necessary)
        ((or `item `plain-list)
         (let ((orig-point (point)))
           ;; Position determines where org-insert-todo-heading and `org-insert-item'
           ;; insert the new list item.
           (if (eq direction 'above)
               (org-beginning-of-item)
             (end-of-line))
           (let* ((ctx-item? (eq 'item (org-element-type context)))
                  (ctx-cb (org-element-property :contents-begin context))
                  ;; Hack to handle edge case where the point is at the
                  ;; beginning of the first item
                  (beginning-of-list? (and (not ctx-item?)
                                           (= ctx-cb orig-point)))
                  (item-context (if beginning-of-list?
                                    (org-element-context)
                                  context))
                  ;; Horrible hack to handle edge case where the
                  ;; line of the bullet is empty
                  (ictx-cb (org-element-property :contents-begin item-context))
                  (empty? (and (eq direction 'below)
                               ;; in case contents-begin is nil, or contents-begin
                               ;; equals the position end of the line, the item is
                               ;; empty
                               (or (not ictx-cb)
                                   (= ictx-cb
                                      (1+ (point))))))
                  (pre-insert-point (point)))
             ;; Insert dummy content, so that `org-insert-item'
             ;; inserts content below this item
             (when empty?
               (insert " "))
             (org-insert-item (org-element-property :checkbox context))
             ;; Remove dummy content
             (when empty?
               (delete-region pre-insert-point (1+ pre-insert-point))))))
        ;; Add a new table row
        ((or `table `table-row)
         (pcase direction
           ('below (save-excursion (org-table-insert-row t))
                   (org-table-next-row))
           ('above (save-excursion (org-shiftmetadown))
                   (+org/table-previous-row))))

        ;; Otherwise, add a new heading, carrying over any todo state, if
        ;; necessary.
        (_
         (let ((level (or (org-current-level) 1)))
           ;; I intentionally avoid `org-insert-heading' and the like because they
           ;; impose unpredictable whitespace rules depending on the cursor
           ;; position. It's simpler to express this command's responsibility at a
           ;; lower level than work around all the quirks in org's API.
           (pcase direction
             (`below
              (let (org-insert-heading-respect-content)
                (goto-char (line-end-position))
                (org-end-of-subtree)
                (insert "\n" (make-string level ?*) " ")))
             (`above
              (org-back-to-heading)
              (insert (make-string level ?*) " ")
              (save-excursion (insert "\n"))))
           (run-hooks 'org-insert-heading-hook)
           (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                       (todo-type    (org-element-property :todo-type context)))
             (org-todo
              (cond ((eq todo-type 'done)
                     ;; Doesn't make sense to create more "DONE" headings
                     (car (+org-get-todo-keywords-for todo-keyword)))
                    (todo-keyword)
                    ('todo)))))))

      (when (org-invisible-p)
        (org-show-hidden-entry))
      (when (and (bound-and-true-p evil-local-mode)
                 (not (evil-emacs-state-p)))
        (evil-insert 1))))

  (defun +org/insert-item-below (count)
    "Inserts a new heading, table cell or item below the current one."
    (interactive "p")
    (dotimes (_ count) (+org--insert-item 'below)))
  (defun +org/insert-item-above (count)
    "Inserts a new heading, table cell or item above the current one."
    (interactive "p")
    (dotimes (_ count) (+org--insert-item 'above)))

  ;; END from doom emacs

  (setq org-startup-indented t)
  (setq org-special-ctrl-a/e t)
  (setq org-blank-before-new-entry nil)

  (setq org-priority-default ?0)
  (setq org-priority-highest ?A)
  (setq org-priority-lowest ?D)
  (setq org-priority-faces '((?A . org-level-1)
                             (?B . org-level-2)
                             (?C . org-level-3)
                             (?D . org-level-4)))
  (setq org-priority-start-cycle-with-default t)

  (setq org-agenda-files '("inbox.org" "agenda.org" "temp_agenda.org"))
  (setq org-agenda-prefix-format '((agenda . " %i %?-12t%-3s%2e ")
                                   (todo . " %i%3e ")
                                   (tags . " %i")
                                   (search . " %i")))
  (setq org-agenda-time-grid '((daily today remove-match)
                               (900 930 1000 1030 1100 1130 1200 1230 1300 1330 1400 1430 1500 1530 1600 1630 1700 1730 1800 1830 1900 1930 2000 2030 2100 2130 2200 2230 2300 2330)
                               " ┄┄┄┄┄ " ""))
  (setq org-agenda-scheduled-leaders '("S:" "!%d"))
  (setq org-agenda-deadline-leaders '("D:" "-%d" "%2dd ago: "))
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-window-setup 'current-window)
  (setq org-deadline-warning-days 7)
  (setq org-modules '(org-habit))
  (setq org-time-stamp-rounding-minutes '(0 30))
  (setq org-habit-show-habits-only-for-today nil)
  (setq org-habit-show-done-always-green t)
  (setq org-habit-following-days 3)
  (setq +org-habit-graph-window-ratio 0.2)
  (setq org-extend-today-until 3)

  (defun my/org-agenda-custom-sort (a b)
    "Like the `time-up' sorting strategy, but keep timestamps last.

This is very similar to the `time-up' options for `org-agenda-sorting-strategy',
but it always sorts agenda items without a timestamp first (before any items
with a timestamp).

Also sorts items with a deadline after scheduled items."
    (let ((a-timep (get-text-property 1 'time-of-day a))
          (b-timep (get-text-property 1 'time-of-day b))
          (a-type (get-text-property 1 'type a))
          (b-type (get-text-property 1 'type b))
          (a-todo-state (get-text-property 1 'todo-state a))
          (b-todo-state (get-text-property 1 'todo-state b)))
      (cond
       ((and a-timep b-timep) (org-cmp-time a b))
       (a-timep +1)
       (b-timep -1)
       ((and (string= a-type "upcoming-deadline")
             (not (string= b-type "upcoming-deadline"))) +1)
       ((and (not (string= a-type "upcoming-deadline"))
             (string= b-type "upcoming-deadline")) -1))))

  (setq org-agenda-sorting-strategy '((agenda user-defined-up deadline-up priority-down scheduled-up todo-state-up effort-up habit-down)
                                      (todo todo-state-up priority-down deadline-up scheduled-up ts-up effort-up tag-up)
                                      (tags todo-state-up priority-down deadline-up ts-up effort-up)
                                      (search scheduled-up priority-down todo-state-up effort-up)))
  (setq org-agenda-cmp-user-defined #'my/org-agenda-custom-sort)

  (setq org-agenda-custom-commands '(("d" "Daily agenda and TODOs"
                                      ((todo "TODO" ((org-agenda-overriding-header "Inbox")
                                                     (org-agenda-files '("inbox.org"))))
                                       (agenda "" ((org-agenda-span 3)
                                                   (org-agenda-start-day "0d")
                                                   (org-agenda-dim-blocked-tasks nil)))))
                                     ("g" "Time grid and TODOs for 3 days with effort sums"
                                      ((agenda "" ((org-agenda-span 1)
                                                   (org-agenda-start-day "0d")
                                                   (org-agenda-dim-blocked-tasks nil)))
                                       (agenda "" ((org-agenda-span 1)
                                                   (org-agenda-start-day "+1d")
                                                   (org-agenda-dim-blocked-tasks nil)))
                                       (agenda "" ((org-agenda-span 1)
                                                   (org-agenda-start-day "+2d")
                                                   (org-agenda-dim-blocked-tasks nil)))))
                                     ("D" "Daily TODOs for a week"
                                      ((agenda "" ((org-agenda-overriding-header "Nonhabits")
                                                   (org-agenda-span 7)
                                                   (org-agenda-start-day "0d")
                                                   (org-agenda-dim-blocked-tasks nil)
                                                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("HABT")))
                                                   (org-agenda-use-time-grid nil)))))
                                     ("c" "Calendar"
                                      ((agenda "" ((org-agenda-overriding-header "Calendar")
                                                   (org-agenda-span 7)
                                                   (org-agenda-start-day "0d")
                                                   (org-agenda-dim-blocked-tasks nil)
                                                   (org-agenda-use-time-grid nil)
                                                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("HABT" "LOOP")))))))
                                     ("x" "test"
                                      ((agenda "" ((org-agenda-span 1)
                                                   (org-agenda-start-day "0d")
                                                   (org-agenda-use-time-grid nil)))))
                                     ("tp" "Project TODOs"
                                      ((tags-todo "projects+TODO=\"TODO\""
                                                  ((org-agenda-overriding-header "Project TODOs")))
                                       (todo "PROJ" ((org-agenda-overriding-header "Projects")
                                                     (org-agenda-dim-blocked-tasks nil)))))
                                     ("te" "Entertainment"
                                      ((tags-todo "entertainment+TODO=\"TODO\""
                                                  ((org-agenda-overriding-header "Entertainment TODOs")))))
                                     ("ta" "Fine Arts"
                                      ((tags-todo "arts+TODO=\"TODO\""
                                                  ((org-agenda-overriding-header "Fine Arts TODOs")))))
                                     ("w" "Week-long daily agenda"
                                      ((agenda "" ((org-agenda-span 1) (org-agenda-start-day "0d")))
                                       (agenda "" ((org-agenda-span 1) (org-agenda-start-day "+1d")))
                                       (agenda "" ((org-agenda-span 1) (org-agenda-start-day "+2d")))
                                       (agenda "" ((org-agenda-span 1) (org-agenda-start-day "+3d")))
                                       (agenda "" ((org-agenda-span 1) (org-agenda-start-day "+4d")))
                                       (agenda "" ((org-agenda-span 1) (org-agenda-start-day "+5d")))
                                       (agenda "" ((org-agenda-span 1) (org-agenda-start-day "+6d")))
                                       (agenda "" ((org-agenda-span 1) (org-agenda-start-day "+7d")))))))

  (setq org-log-into-drawer t)
  (setq org-log-reschedule 'time)
  (setq +org-capture-todo-file "inbox.org")
  (setq org-capture-templates '(("T" "Immediate todo" entry
                                 (file +org-capture-todo-file)
                                 "* TODO %?\n%U\n%i")))
  (setq org-archive-location "agenda_archive.org::")

  (setq org-use-fast-todo-selection 'expert)

  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-someday '((t (:inherit (bold font-lock-comment-face org-todo)))) "" ))
  (setq org-todo-keywords '((sequence "TODO(t)" "TEMP(e)" "PROJ(p@)" "LOOP(l!)" "HABT(h!)" "WAIT(w@/@)" "IDEA(i)" "SOMEDAY(m)" "NOTE(o)" "|" "DONE(d!)" "KILL(k@)")))
  (setq org-todo-repeat-to-state t)
  (setq org-todo-keyword-faces '(("TODO" . org-todo)
                                 ("TEMP" . org-level-2)
                                 ("PROJ" . org-level-1)
                                 ("LOOP" . +org-todo-active)
                                 ("HABT" . org-table)
                                 ("WAIT" . org-level-4)
                                 ("IDEA" . +org-todo-project)
                                 ("SOMEDAY" . +org-todo-someday)
                                 ("KILL" . org-agenda-dimmed-todo-face)
                                 ("NOTE" . org-agenda-dimmed-todo-face)))


  )

(use-package evil-nerd-commenter
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  :general
  ([remap comment-line] #'evilnc-comment-or-uncomment-lines)
  (general-define-key
   :states 'motion
   :keymaps 'override
   "gc" #'evilnc-comment-operator)
  )

(use-package evil-org
  :demand t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (setq evil-org-key-theme '(navigation insert textobjects additional calendar))
  (evil-org-set-key-theme)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  ; have to do this to override the default evil-org definitions
  (evil-define-key 'normal 'evil-org-mode (kbd "C-<return>") #'+org/insert-item-below)
  (evil-define-key 'normal 'evil-org-mode (kbd "C-S-<return>") #'+org/insert-item-above)
  (evil-define-key 'insert 'evil-org-mode (kbd "C-<return>") #'+org/insert-item-below)
  (evil-define-key 'insert 'evil-org-mode (kbd "C-S-<return>") #'+org/insert-item-above)
  (evil-define-key 'motion 'org-agenda-mode-map "H" #'org-agenda-date-earlier-minutes)
  (evil-define-key 'motion 'org-agenda-mode-map "L" #'org-agenda-date-later-minutes)
  )

(use-package evil-surround
  :demand t
  :config
  (global-evil-surround-mode 1))

(use-package orderless
  :init
  (setq orderless-matching-styles '(orderless-initialism
                                    orderless-literal
                                    orderless-regexp)
        ; not sure why this is needed, but gives error if this isn't set at all
        completion-lazy-hilit t)
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package moody
  :demand t
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package evil-visualstar
  :demand t
  :config
  (global-evil-visualstar-mode))

(use-package magit
  :general
  (leader-def
    "gg" #'magit-status
    )
  :config
  (setq magit-delete-by-moving-to-trash nil)
  ;; :custom-face
  ;; (magit-diff-revision-summary-highlight ((t (:weight bold :inherit magit-section-secondary-heading))))
  ;; (magit-diff-revision-summary ((t (:weight semi-bold :inherit magit-diff-revision-summary-highlight))))
  )

(use-package forge
  :after magit
  :general
  ('magit-status-mode-map "@" #'forge-dispatch))

(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Calendar\\*"
          help-mode
          helpful-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(use-package gcmh
  :config
  (setq gcmh-high-cons-threshold (* 16 1024 1024))
  :hook
  (after-init . gcmh-mode))

(use-package profiler
  :general
  (general-define-key
    :states 'normal
    :keymaps 'profiler-report-mode-map
    "TAB" #'profiler-report-toggle-entry
    "<return>" #'profiler-report-toggle-entry
    "i" #'profiler-report-toggle-entry)
  (leader-def
    "hTs" #'profiler-start
    "hTt" #'profiler-stop
    "hTr" #'profiler-report))

(use-package beancount
  :config

  ;; Defined in ~/.emacs.d/modules/lang/beancount/autoload.el

  (defun +beancount/balance ()
    "Display a balance report with bean-report (bean-report bal)."
    (interactive)
    (let (compilation-read-command)
      (beancount--run "bean-report" buffer-file-name "bal")))

  (defun +beancount/clone-transaction ()
    "Clones a transaction from (and to the bottom of) the current ledger buffer.

    Updates the date to today."
    (interactive)
    (save-restriction
      (widen)
      (when-let (transaction
                  (completing-read
                    "Clone transaction: "
                    (string-lines (buffer-string))
                    (apply-partially #'string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [*!] ")
                    t))
                (goto-char (point-min))
                (re-search-forward (concat "^" (regexp-quote transaction)))
                (+beancount/clone-this-transaction t))))

  (defun +beancount/clone-this-transaction (&optional arg)
    "Clones the transaction at point to the bottom of the ledger.

    Updates the date to today."
    (interactive "P")
    (if (and (not arg) (looking-at-p "^$"))
      (call-interactively #'+beancount/clone-transaction)
      (save-restriction
        (widen)
        (let ((transaction
                (buffer-substring-no-properties
                  (save-excursion
                    (beancount-goto-transaction-begin)
                    (re-search-forward " [!*] " nil t)
                    (point))
                  (save-excursion
                    (beancount-goto-transaction-end)
                    (point)))))
          (goto-char (point-max))
          (delete-blank-lines)
          (newline)
          (beancount-insert-date)
          (insert " ! ")
          (insert transaction)))))


  (general-define-key
    :prefix my-leader
    :non-normal-prefix my-insert-leader
    :keymaps 'beancount-mode-map
    :states '(normal visual motion insert emacs)
    "mid" #'beancount-insert-date
    "mic" #'+beancount/clone-transaction
    "miC" #'+beancount/clone-this-transaction
    "mb" #'+beancount/balance
    "mc" #'beancount-check
    "mx" #'beancount-context
    )
  )
