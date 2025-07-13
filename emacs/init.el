; -*- lexical-binding: t; -*-

; (setq use-package-compute-statistics t)

;; Checklist for things that constantly break (check when changing config):
;; - `gc` opens calendar in org-agenda, but comments lines elesewhere
;;   - `C-l`, etc. keys to move around in calendar
;; - minibuffer:
;;   - `esc` immediate exits
;;   - `C-w` and `C-u` work as expected
;;   - `C-f` enters normal mode

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


(use-package delight
  :demand t)


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

  ; fix macos keybindings
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'super)
    (setq mac-option-modifier 'meta)
    (setq mac-control-modifier 'control))

  (setq create-lockfiles nil
        make-backup-files nil
        vc-follow-symlinks nil
        compilation-window-height 20
        auto-save-default t
        ;; Don't auto-disable auto-save after deleting big chunks. This defeats
        ;; the purpose of a failsafe. This adds the risk of losing the data we
        ;; just deleted, but I believe that's VCS's jurisdiction, not ours.
        auto-save-include-big-deletions t
        ;; Keep it out of `doom-emacs-dir' or the local directory.
        auto-save-list-file-prefix (expand-file-name "saves/autosave" user-emacs-directory)
        tramp-auto-save-directory  (expand-file-name "saves/tramp-autosave" user-emacs-directory)
        ;; source: https://github.com/akermu/emacs-libvterm/issues/369#issuecomment-658538400
        tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"
        auto-save-file-name-transforms
        (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                    ;; Prefix tramp autosaves to prevent conflicts with local ones
                    (concat auto-save-list-file-prefix "tramp-\\2") t)
              (list ".*" auto-save-list-file-prefix t)))

  (set-frame-font (font-spec
                   :family "iosevka term ss07"
                   :width 'expanded
                   :size 15
                   ))
  (set-face-attribute 'default nil :height 150)

  (add-to-list 'safe-local-variable-values '(eval auto-revert-mode 1))
  (add-to-list 'safe-local-variable-values '(display-line-numbers . visual))

  (setq display-line-numbers-type 'relative)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'text-mode-hook #'display-line-numbers-mode)

  (setq-default show-trailing-whitespace nil)
  ;; disable trailing whitespace in certain modes
  (defun my/hide-trailing-whitespace ()
    (setq-local show-trailing-whitespace nil))
  (add-hook 'gptel-mode-hook #'my/hide-trailing-whitespace)
  (add-hook 'org-agenda-mode-hook #'my/hide-trailing-whitespace)
  (add-hook 'calendar-mode-hook #'my/hide-trailing-whitespace)
  (add-hook 'vterm-mode-hook #'my/hide-trailing-whitespace)

  (add-hook 'text-mode-hook #'visual-line-mode)

  (setq project-switch-commands #'project-find-file)

  (tab-bar-mode)
  (setq tab-bar-new-tab-choice "*new-tab*")

  (setq browse-url-browser-function #'browse-url-default-browser)
  (setq browse-url-secondary-browser-function #'xwidget-webkit-browse-url)
  (setq url-privacy-level 'high)

  (setq project-vc-merge-submodules nil)
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

(defun my/kill-all ()
  "Kill all buffers in buffer-list and cd back to home"
  (interactive)
  (tramp-cleanup-all-connections)
  (and (fboundp 'eglot-shutdown-all)
       ;; shutdown all eglot servers
       (eglot-shutdown-all))
  (ignore-errors (mapc #'kill-buffer (buffer-list)))
  (cd "~/")
  (delete-other-windows)
  (tab-bar-close-other-tabs)
  (tab-bar-close-tab))

(defun my/default-agenda-view ()
  "Open my personal split screen agenda view"
  (interactive)
  (delete-other-windows)
  ;; disable popup for file selection in project
  (setq current-prefix-arg t)
  ;; switch to org-directory project first to avoid projectile issues
  ;; (projectile-switch-project-by-name org-directory)
  (find-file (concat org-directory "inbox.org"))
  (find-file (concat org-directory "agenda.org"))
  ;; open up org-agenda and agenda.org side by side
  (evil-window-vsplit)
  (org-agenda nil "d")
  ;; ugly hack to refresh org-agenda after inline links are rendered
  (sleep-for 0.01)
  (org-agenda-redo)
  )

(defun my/delete-this-file ()
  "Delete the file associated with the current buffer and kill the buffer."
  (interactive)
  (if (buffer-file-name)
      (progn
        (delete-file (buffer-file-name))
        (kill-buffer (current-buffer))
        (message "File deleted and buffer killed."))
    (message "Buffer is not associated with a file.")))

(defun my/vterm-in-home (&optional arg)
  "Start a vterm session in my home directory."
  (interactive "P")
  (let ((default-directory (getenv "HOME")))
    (call-interactively #'vterm)))

(defun my/vterm-new-tab ()
  "Open a new vterm buffer and switch to it in a new tab."
  (interactive)
  (let ((vterm-buffer (vterm)))
    (switch-to-buffer-other-tab vterm-buffer)))

;; adapted from: https://github.com/doomemacs/doomemacs/blob/2bc052425ca45a41532be0648ebd976d1bd2e6c1/modules/config/default/autoload/text.el#L42-L55
;; see also: https://github.com/doomemacs/doomemacs/blob/2bc052425ca45a41532be0648ebd976d1bd2e6c1/modules/config/default/%2Bevil-bindings.el#L437
(defun my/yank-buffer-path (&optional root)
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let* ((filename (or (buffer-file-name (buffer-base-buffer))
                          (bound-and-true-p list-buffers-directory))))
      (let ((path (abbreviate-file-name
                   (if root
                       (file-relative-name filename root)
                     filename))))
        (kill-new path)
        (if (string= path (car kill-ring))
            (message "Copied path: %s" path)
          (user-error "Couldn't copy filename in current buffer")))
    (error "Couldn't find filename in current buffer")))

;; adapted from: https://github.com/doomemacs/doomemacs/blob/2bc052425ca45a41532be0648ebd976d1bd2e6c1/modules/config/default/autoload/text.el#L58-L65
(defun my/yank-buffer-path-relative-to-project (&optional include-root)
  "Copy the current buffer's path to the kill ring.
With non-nil prefix INCLUDE-ROOT, also include the project's root."
  (interactive "P")
  (my/yank-buffer-path
   (when-let* ((proj (project-current))
               (proj-root (project-root proj))
               (proj-root-path (file-name-directory (directory-file-name proj-root))))
     (if include-root
         proj-root-path
       proj-root))))

;; (defun my/kill-non-project-buffers ()
;;   "Kill all buffers that are not associated with a project."
;;   (interactive)
;;   (dolist (buffer (buffer-list))
;;     (with-current-buffer buffer
;;       (unless (project-current)
;;         (kill-buffer buffer)))))

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

  ;; doomesque hotkeys using spacebar as prefix
  (leader-def
    ;; map universal argument to SPC-u
    "u" #'universal-argument
    "." #'find-file
    ";" #'pp-eval-expression
    "`" #'evil-switch-to-windows-last-buffer

    "bi" #'ibuffer
    "bk" #'kill-current-buffer
    "bK" #'kill-buffer
    "br" #'revert-buffer-quick
    "d" #'kill-current-buffer

    ;; open init.el file
    "ei" (lambda () (interactive) (find-file "~/config/emacs/init.el"))
    "eg" #'gptel
    "ev" #'eval-last-sexp

    "fd" #'my/delete-this-file
    "fr" #'rename-visited-file
    "fy" #'my/yank-buffer-path
    "fY" #'my/yank-buffer-path-relative-to-project

    "he" #'view-echo-area-messages
    "hl" #'view-lossage
    "hm" #'describe-mode
    "ht" #'load-theme

    "na" #'org-agenda
    "nt" #'org-todo-list
    "nl" #'org-store-link
    "n SPC" #'my/default-agenda-view

    "ot" #'vterm
    "oT" #'my/vterm-in-home
    "on" #'my/vterm-new-tab
    "ow" #'xwidget-webkit-browse-url

    "pk" #'project-kill-buffers
    "pK" #'my/kill-non-project-buffers
    "pp" #'project-switch-project
    "SPC" #'project-find-file

    "qq" #'save-buffers-kill-emacs
    "qQ" #'kill-emacs
    "qa" #'my/kill-all
    "qr" #'restart-emacs
    "qp" #'kill-process

    "tw" #'visual-line-mode
    "tt" #'toggle-truncate-lines
    "td" #'(lambda () (interactive) (load-theme 'doom-one t))
    "tl" #'(lambda () (interactive)
             (if (eq display-line-numbers-type 'relative)
                 (setq display-line-numbers-type t)
               (setq display-line-numbers-type 'relative))
             (display-line-numbers-mode))
    "tL" #'(lambda () (interactive) (load-theme 'doom-one-light t))

    ; cd to current file's directory
    "cd" #'(lambda () (interactive) (cd (file-name-directory (buffer-file-name))))
    "cw" #'delete-trailing-whitespace
    "c]" #'flymake-goto-next-error
    "c[" #'flymake-goto-prev-error
    "cx" #'flymake-show-project-diagnostics
    "cd" #'evil-goto-definition
    "cll" #'eglot
    "cls" #'eglot-shutdown

    "X" #'org-capture
    "x" #'scratch-buffer

    "1" #'(lambda () (interactive) (tab-bar-select-tab 1))
    "2" #'(lambda () (interactive) (tab-bar-select-tab 2))
    "3" #'(lambda () (interactive) (tab-bar-select-tab 3))
    "4" #'(lambda () (interactive) (tab-bar-select-tab 4))
    "5" #'(lambda () (interactive) (tab-bar-select-tab 5))
    "6" #'(lambda () (interactive) (tab-bar-select-tab 6))
    "7" #'(lambda () (interactive) (tab-bar-select-tab 7))
    "8" #'(lambda () (interactive) (tab-bar-select-tab 8))
    "9" #'tab-bar-switch-to-last-tab
    "TAB 1" #'(lambda () (interactive) (tab-bar-select-tab 1))
    "TAB 2" #'(lambda () (interactive) (tab-bar-select-tab 2))
    "TAB 3" #'(lambda () (interactive) (tab-bar-select-tab 3))
    "TAB 4" #'(lambda () (interactive) (tab-bar-select-tab 4))
    "TAB 5" #'(lambda () (interactive) (tab-bar-select-tab 5))
    "TAB 6" #'(lambda () (interactive) (tab-bar-select-tab 6))
    "TAB 7" #'(lambda () (interactive) (tab-bar-select-tab 7))
    "TAB 8" #'(lambda () (interactive) (tab-bar-select-tab 8))
    "TAB 9" #'tab-bar-switch-to-last-tab
    "TAB TAB" #'tab-list
    "TAB c" #'tab-bar-new-tab
    "TAB d" #'tab-bar-close-tab
    "TAB D" #'(lambda () (interactive) (kill-current-buffer) (tab-bar-close-tab))
    "TAB m" #'tab-bar-move-tab
    "TAB M" #'tab-bar-move-tab-to
    "TAB n" #'tab-bar-switch-to-next-tab
    "TAB p" #'tab-bar-switch-to-prev-tab
    )
  (general-define-key
    :keymaps 'universal-argument-map
    :prefix my-leader
    :non-normal-prefix my-insert-leader
    "u" #'universal-argument-more)
  (general-define-key
    :states 'normal
    "g C-g" #'count-words
    ;; switch to most recently used tab
    "C-M-6" #'tab-bar-switch-to-recent-tab
    "C-s-6" #'tab-bar-switch-to-recent-tab
    )
  (general-define-key
    "s-w" #'kill-current-buffer
    "s-s" #'save-buffer
    "s-v" #'yank
    "s-1" #'(lambda () (interactive) (tab-bar-select-tab 1))
    "s-2" #'(lambda () (interactive) (tab-bar-select-tab 2))
    "s-3" #'(lambda () (interactive) (tab-bar-select-tab 3))
    "s-4" #'(lambda () (interactive) (tab-bar-select-tab 4))
    "s-5" #'(lambda () (interactive) (tab-bar-select-tab 5))
    "s-6" #'(lambda () (interactive) (tab-bar-select-tab 6))
    "s-7" #'(lambda () (interactive) (tab-bar-select-tab 7))
    "s-8" #'(lambda () (interactive) (tab-bar-select-tab 8))
    "s-9" #'tab-bar-switch-to-last-tab
    "s-}" #'tab-bar-switch-to-next-tab
    "s-{" #'tab-bar-switch-to-prev-tab
    "C-<tab>" #'tab-bar-switch-to-next-tab
    "C-S-<tab>" #'tab-bar-switch-to-prev-tab
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
    "hk" #'helpful-key
    "ha" #'apropos)
  (general-define-key
   :keymaps 'helpful-mode-map
   :states 'normal
   "q" #'quit-window)
  )

(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  ;; these need to be set for evil-collection
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)

  (setq evil-want-C-u-scroll t
        evil-want-C-u-delete t
        ; disable evil in minibuffer on startup
        evil-want-minibuffer nil
        evil-want-keybinding nil
        evil-want-Y-yank-to-eol t
        evil-symbol-word-search t
        evil-search-module 'evil-search
        evil-undo-system 'undo-redo)
  (defun my/toggle-window-maximize ()
    "Temporarily maximize the buffer"
    (interactive)
    (if (= 1 (length (window-list)))
        (jump-to-register '_)
      (progn
        (window-configuration-to-register '_)
        (delete-other-windows))))
  :config
  (leader-def
    "ww" #'evil-window-mru
    "wh" #'evil-window-left
    "wl" #'evil-window-right
    "wk" #'evil-window-up
    "wj" #'evil-window-down
    "wv" #'evil-window-vsplit
    "ws" #'evil-window-split
    "wd" #'evil-window-delete
    "wz" #'my/toggle-window-maximize
    )
  (general-define-key
    :kemaps 'minibuffer-mode-map
    "C-u" #'evil-delete-back-to-indentation
    ; some convenience "evil mode" keys in minibuffer directly
    "C-w" #'evil-delete-backward-word
    ; allow entering evil with C-f in minibuffer
    "C-f" #'evil-normal-state)
  (evil-mode 1)
  (evil-define-key 'normal help-mode-map (kbd "TAB") #'forward-button)
  )

(use-package evil-collection
  :delight evil-collection-unimpaired-mode
  :after evil
  :demand t
  :config
  (evil-collection-init))

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
  :delight
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
    "sb" #'consult-line-multi
    "si" #'consult-imenu
    "," #'consult-buffer
    "p," #'consult-project-buffer
    "<" #'consult-buffer-all
    "/" #'consult-ripgrep)

  :config

  (defvar consult--source-buffer-no-hidden
    `( :name     "Buffer (no hidden)"
       :narrow   ?\s
       :category buffer
       :face     consult-buffer
       :history  buffer-name-history
       :state    ,#'consult--buffer-state
       :default  t
       :items
       ,(lambda () (consult--buffer-query
                    :sort 'visibility
                    :as #'consult--buffer-pair
                    :predicate (lambda (buf)
                                 (cond
                                  ;; allow certain prefixes
                                  ((let* ((allowed-prefixes '("*Org Agenda" "*vterm" "*xwidget-webkit"))
                                          (buf-name (buffer-name buf))
                                          (allowed-prefix-p (lambda (prefix) (string-prefix-p prefix buf-name))))
                                     (cl-some allowed-prefix-p allowed-prefixes)) t)
                                  ;; filter out hidden buffers (beginning with "*")
                                  ((and (string-prefix-p "*" (buffer-name buf))
                                        (string-suffix-p "*" (buffer-name buf)))
                                   nil)
                                  ;; filter out magit buffers
                                  ((string-prefix-p "magit" (buffer-name buf)) nil)
                                  (t t))))))
    "Buffer candidates without most hidden buffers.")

  ;; added :hidden t to default consult--source-buffer
  (defvar consult--source-buffer-all
    `( :name     "Buffer (all)"
       :narrow   ?b
       :hidden   t
       :category buffer
       :face     consult-buffer
       :history  buffer-name-history
       :state    ,#'consult--buffer-state
       :default  t
       :items
       ,(lambda () (consult--buffer-query :sort 'visibility
                                          :as #'consult--buffer-pair)))
    "Buffer candidates with all buffers.")

  ;; removed :hidden t from default consult--source-modified-buffer
  (setq consult--source-modified-buffer
    `( :name     "Modified Buffer"
       :narrow   ?*
       :category buffer
       :face     consult-buffer
       :history  buffer-name-history
       :state    ,#'consult--buffer-state
       :items
       ,(lambda () (consult--buffer-query :sort 'visibility
                                          :as #'consult--buffer-pair
                                          :predicate
                                          (lambda (buf)
                                            (and (buffer-modified-p buf)
                                                 (buffer-file-name buf)))))))

  (setq consult-buffer-sources '(consult--source-modified-buffer
                                 consult--source-buffer-no-hidden
                                 consult--source-buffer-all
                                 consult--source-recent-file
                                 consult--source-file-register
                                 consult--source-bookmark))

  (defun consult-buffer-all (&rest all)
    "Wrapper around consult-buffer which shows all buffer sources."
    (interactive)
    (let ((consult-buffer-sources
           '(consult--source-modified-buffer
             consult--source-buffer
             consult--source-recent-file
             consult--source-file-register
             consult--source-bookmark)))
      (apply #'consult-buffer all)))
  )

(use-package recentf
  :demand t
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-saved-items 100
        recentf-exclude '("/tmp/")))

(use-package org
  :demand t
  :straight (org :host github
                 :repo "richyliu/org-mode")
  :init
  (setq org-directory "/Users/richard/Documents/org/agenda/")
  (setq org-agenda-files '("inbox.org" "agenda.org"))

  :general
  (defun my/temp-refile ()
    "Refile item to heading '*Temporary' in file 'agenda.org'"
    (interactive)
    (org-schedule nil "+0d")
    (org-todo "TEMP")
    (let* ((org-buffer (get-buffer "agenda.org"))
           (match-target-fn (lambda (refloc) (string-match "agenda.org/Temporary"
                                                           (car refloc))))
           (target-rfloc (cl-find-if match-target-fn
                                     (org-refile-get-targets org-buffer))))
      (org-refile nil nil target-rfloc nil)))

  (defun +org/dwim-at-point (&optional arg)
    "Do-what-I-mean at point. Inspired by doom emacs function of the same name.

    If on a:
    - link: follow it
    - otherwise, do nothing."
    (interactive "P")
    (if (button-at (point))
      (call-interactively #'push-button)
      (let* ((context (org-element-context))
             (type (org-element-type context)))
        ;; skip over unimportant contexts
        (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
               (setq context (org-element-property :parent context)
                     type (org-element-type context)))
        (pcase type
               (`link (org-open-at-point arg))

               (_
                 (if (or (org-in-regexp org-ts-regexp-both nil t)
                         (org-in-regexp org-tsr-regexp-both nil  t)
                         (org-in-regexp org-link-any-re nil t))
                   (call-interactively #'org-open-at-point)))))))

  (defun my/org-copy-pair-inc-date ()
    "Duplicate two headings (while on the second one) and increment date of both"
    (interactive)
    (org-clone-subtree-with-time-shift 1 "+1d")
    (org-backward-element)
    (org-clone-subtree-with-time-shift 1 "+1d")
    (org-forward-element)
    (org-metadown)
    (org-forward-element))

  (defun my/org-open-link-secondary-browser ()
    "Open link at point in secondary browser"
    (interactive)
    (let ((browse-url-browser-function browse-url-secondary-browser-function))
      (org-open-at-point)))

  (general-define-key
    :prefix my-leader
    :non-normal-prefix my-insert-leader
    :keymaps 'org-mode-map
    :states '(normal visual motion insert emacs)
    "mcE" #'org-set-effort
    "mdd" #'org-deadline
    "mds" #'org-schedule
    "mdt" #'org-time-stamp
    "mdT" #'org-time-stamp-inactive
    "mpp" #'org-priority
    ;; clear the priority
    "mpc" (lambda () (interactive) (org-priority 'remove))

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
    ;; "mss" #'org-sparse-tree
    "msA" #'org-archive-subtree-default
    "msN" #'widen
    "msP" #'my/org-copy-pair-inc-date

    "mrr" #'org-refile

    "mgr" #'org-refile-goto-last-stored

    "mlt" #'org-toggle-link-display
    "mlg" #'my/org-open-link-secondary-browser

    "m'" #'org-edit-special
    "m," #'org-switchb
    "m." #'consult-org-heading
    "m/" #'consult-org-agenda
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
    "<return>" #'+org/dwim-at-point
    )
  (general-define-key
    :states '(motion insert)
    :keymaps 'org-mode-map
    "s-k" #'org-insert-link)
  (general-define-key
    :states 'motion
    :keymaps 'org-agenda-mode-map
    "cs" #'org-agenda-schedule
    "cd" #'org-agenda-deadline
    "s-s" #'org-save-all-org-buffers
    "s-r" #'org-agenda-redo
    "r" #'org-agenda-redo

    "gc" #'org-agenda-goto-calendar
    )
  (general-define-key
    :keymaps 'org-read-date-minibuffer-local-map
    "C-h" #'(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1)))
    "C-l" #'(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1)))
    "C-k" #'(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1)))
    "C-j" #'(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))
  (general-define-key
    :keymaps 'calendar-mode-map
    "C-h" #'(lambda () (interactive) (calendar-backward-day 1))
    "C-l" #'(lambda () (interactive) (calendar-forward-day 1))
    "C-k" #'(lambda () (interactive) (calendar-backward-week 1))
    "C-j" #'(lambda () (interactive) (calendar-forward-week 1)))

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
             ;; (my modification) Don't split the line so that we can
             ;; insert past any children. I don't know why this is
             ;; tied to the org-M-RET-may-split-line variable, but it
             ;; seems like allowing for split lines forces the list
             ;; item to be inserted immediately after the current one.
             ;; See also: https://emacs.stackexchange.com/a/79066
             (let ((org-M-RET-may-split-line nil))
                (org-insert-item (org-element-property :checkbox context)))
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

  (setq org-indirect-buffer-display 'current-window
        org-enforce-todo-dependencies t
        org-entities-user
        '(("flat"  "\\flat" nil "" "" "266D" "♭")
          ("sharp" "\\sharp" nil "" "" "266F" "♯"))
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-hide-leading-stars t
        org-image-actual-width nil
        org-imenu-depth 6
        org-tags-column 0
        org-use-sub-superscripts '{}
        ;; `showeverything' is org's default, but it doesn't respect
        ;; `org-hide-block-startup' (#+startup: hideblocks), archive trees,
        ;; hidden drawers, or VISIBILITY properties. `nil' is equivalent, but
        ;; respects these settings.
        org-startup-folded nil)

  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3))
        ;; Without this, completers like ivy/helm are only given the first level of
        ;; each outline candidates. i.e. all the candidates under the "Tasks" heading
        ;; are just "Tasks/". This is unhelpful. We want the full path to each refile
        ;; target! e.g. FILE/Tasks/heading/subheading
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

  (setq org-priority-default ?0)
  (setq org-priority-highest ?A)
  (setq org-priority-lowest ?D)
  (setq org-priority-faces '((?A . org-level-1)
                             (?B . org-level-2)
                             (?C . org-level-3)
                             (?D . org-level-4)))
  (setq org-priority-start-cycle-with-default t)

  (setq org-agenda-files '("inbox.org" "agenda.org"))
  (setq org-agenda-prefix-format '((agenda . " %i %?-12t%-3s%2e ")
                                   (todo . " %i%3e ")
                                   (tags . " %i")
                                   (search . " %i")))
  (setq org-agenda-time-grid '((daily today remove-match)
                               (900 930 1000 1030 1100 1130 1200 1230 1300 1330 1400 1430 1500 1530 1600 1630 1700 1730 1800 1830 1900 1930 2000 2030 2100 2130 2200 2230 2300 2330)
                               " ┄┄┄┄┄ " ""))
  (setq org-agenda-scheduled-leaders '("S:" "!%d"))
  (setq org-agenda-deadline-leaders '("D:" "-%d" "%2dd ago: "))
  ;; (setq org-agenda-dim-blocked-tasks nil) ; for some reason overrides for this in agenda view don't work
  (setq org-agenda-window-setup 'current-window)
  (setq org-deadline-warning-days 7)
  (setq org-modules '(org-habit))
  (setq org-time-stamp-rounding-minutes '(0 30))
  (setq org-habit-show-habits-only-for-today nil)
  (setq org-habit-show-done-always-green t)
  (setq org-habit-following-days 3)
  (setq +org-habit-graph-window-ratio 0.2)
  (setq org-extend-today-until 3)
  (setq org-use-effective-time t)
  (setq org-M-RET-may-split-line '((default . t)))

  (defun my/org-agenda-custom-sort (a b)
    "Like the `time-up' sorting strategy, but keep deadline items first.

This is very similar to the `time-up' options for `org-agenda-sorting-strategy',
but it always sorts deadline items first, then timestamp items, then everything else."
    (let ((a-timep (get-text-property 1 'time-of-day a))
          (b-timep (get-text-property 1 'time-of-day b))
          (a-type (get-text-property 1 'type a))
          (b-type (get-text-property 1 'type b))
          (a-todo-state (get-text-property 1 'todo-state a))
          (b-todo-state (get-text-property 1 'todo-state b)))
      (cond
       ((and (string= a-type "upcoming-deadline")
             (not (string= b-type "upcoming-deadline"))) +1)
       ((and (not (string= a-type "upcoming-deadline"))
             (string= b-type "upcoming-deadline")) -1)
       ((and a-timep b-timep) (org-cmp-time a b))
       (a-timep -1)
       (b-timep +1)
       )))

  (defun my/org-agenda-skip-entry-if (&rest conditions)
    "Skip entry if any of CONDITIONS is true.
Similar to org-agenda-skip-entry-if, except it supports 'tag and
'nottag, which accept one argument and matches for or against that tag
in the heading."
    (org-back-to-heading t)
    (let* ((end (org-entry-end-position))
           (planning-end (line-end-position 2))
           m)
      (and
       (or (and (memq 'scheduled conditions)
                (re-search-forward org-scheduled-time-regexp planning-end t))
           (and (memq 'notscheduled conditions)
                (not
                 (save-excursion
                   (re-search-forward org-scheduled-time-regexp planning-end t))))
           (and (memq 'deadline conditions)
                (re-search-forward org-deadline-time-regexp planning-end t))
           (and (memq 'notdeadline conditions)
                (not
                 (save-excursion
                   (re-search-forward org-deadline-time-regexp planning-end t))))
           (and (memq 'timestamp conditions)
                (re-search-forward org-ts-regexp end t))
           (and (memq 'nottimestamp conditions)
                (not (save-excursion (re-search-forward org-ts-regexp end t))))
           (and (setq m (memq 'regexp conditions))
                (stringp (nth 1 m))
                (re-search-forward (nth 1 m) end t))
           (and (setq m (memq 'notregexp conditions))
                (stringp (nth 1 m))
                (not (save-excursion (re-search-forward (nth 1 m) end t))))
           (and (or
                 (setq m (memq 'nottodo conditions))
                 (setq m (memq 'todo-unblocked conditions))
                 (setq m (memq 'nottodo-unblocked conditions))
                 (setq m (memq 'todo conditions)))
                (org-agenda-skip-if-todo m end))
           (and (setq m (memq 'tag conditions))
                (listp (nth 1 m))
                ;; check if any of the search list of tags is in the heading's tags
                (let ((heading-tags (org-get-tags nil nil))
                      (search-tags (nth 1 m)))
                  (cl-intersection heading-tags search-tags :test #'string=))))
       end)))

  (defvar my/org-spaced-repetition-success 1.4
    "Ratio applied to repeater on success for spaced repetition system.")
  (defvar my/org-spaced-repetition-failure 0.2
    "Ratio applied to repeater on success for spaced repetition system.")
  (defun my/org-spaced-repetition (done-word)
    "Advice for org-auto-repeat-maybe that implements spaced repetition.

Any org item with the SPACED_REPETITION key set to non-nil value will
have all its repeating timestamps affected. The spaced repetition
system will multiply the repeater by my/org-spaced-repetition-success on
every successful (DONE) repetition and by
my/org-spaced-repetition-failure on every unsuccessful (KILL)
repetition, with the minimum being 1. On successful repetition, always
round up. Otherwise, round down."
    (let ((repeat (org-get-repeat))
          (is-success (string= done-word "DONE"))
          (end (copy-marker (org-entry-end-position))))
      (when (and repeat
                 (not (= 0 (string-to-number (substring repeat 1))))
                 (org-entry-get nil "SPACED_REPETITION" t))
        (save-excursion
          (org-back-to-heading t)
          ;; update every repeating timestamp
          (while (re-search-forward org-repeat-re end t)
            (when-let ((new-repeater
                        (save-match-data
                          ;; change the repeater in the timestamp
                          (when-let* ((ts (match-string 0))
                                      (repeater-regexp "\\([.+]\\)?\\(\\+[0-9]+\\)\\([hdwmy]\\)")
                                      (has-repeater (string-match repeater-regexp ts))
                                      (n (string-to-number (match-string 2 ts)))
                                      (new-n (if is-success
                                                 (ceiling (* n my/org-spaced-repetition-success))
                                               (floor (* n my/org-spaced-repetition-failure)))))
                            (format "%s+%d%s" (match-string 1 ts) (max 1 new-n) (match-string 3 ts))))))
              ;; only do this replacement if we have a valid repeater
              ;; note that this replaces the string found with org-repeat-re
              (replace-match new-repeater nil nil nil 1)))))))
  (advice-add 'org-auto-repeat-maybe :before #'my/org-spaced-repetition)

  (setq org-agenda-sorting-strategy '((agenda user-defined-up deadline-up priority-down scheduled-up todo-state-up effort-up habit-down)
                                      (todo todo-state-up priority-down deadline-up ts-up effort-up tag-up)
                                      (tags priority-down todo-state-up deadline-up ts-up effort-up)
                                      (search scheduled-up priority-down todo-state-up effort-up)))
  (setq org-agenda-cmp-user-defined #'my/org-agenda-custom-sort)

  (setq org-agenda-custom-commands '(("d" "Daily agenda and TODOs"
                                      ((todo "TODO" ((org-agenda-overriding-header "Inbox")
                                                     (org-agenda-files '("inbox.org"))))
                                       (agenda "" ((org-agenda-overriding-header "3 days of non-HABTs")
                                                   (org-agenda-span 3)
                                                   (org-agenda-start-day "0d")
                                                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("HABT")))
                                                   (org-agenda-dim-blocked-tasks nil)))
                                       (todo "TODO" ((org-agenda-overriding-header "Low priority todos")
                                                     (org-agenda-files '("agenda.org"))
                                                     (org-agenda-skip-function '(my/org-agenda-skip-entry-if 'scheduled 'tag '("fun")))
                                                     ;; (org-agenda-sorting-strategy '((agenda user-defined-up deadline-up priority-down scheduled-up todo-state-up effort-up habit-down)))
                                                     ))
                                       ))
                                     ("p" "Projects and for fun"
                                      ((todo "PROJ" ((org-agenda-overriding-header "Projects")
                                                     (org-agenda-files '("agenda.org"))
                                                     (org-agenda-dim-blocked-tasks nil)))
                                       (tags "fun"
                                             ;; see https://orgmode.org/manual/Matching-tags-and-properties.html for syntax
                                             ((org-agenda-overriding-header "For fun")
                                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'nottodo '("TODO")))
                                              (org-agenda-dim-blocked-tasks 'invisible)))
                                       (todo "PEND" ((org-agenda-overriding-header "Pending projects")
                                                     ))))
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
                                                   (org-agenda-span 8)
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
                                      ((tags-todo "projects/TODO"
                                                  ((org-agenda-overriding-header "Project TODOs")))
                                       (todo "PROJ" ((org-agenda-overriding-header "Projects")
                                                     (org-agenda-dim-blocked-tasks nil)))))
                                     ("te" "Entertainment"
                                      ((tags-todo "entertainment/TODO"
                                                  ((org-agenda-overriding-header "Entertainment TODOs")))))
                                     ("ta" "Fine Arts"
                                      ((tags-todo "arts/TODO"
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
  (setq +org-capture-todo-file (expand-file-name "inbox.org" org-directory))
  (setq +org-capture-journal-file (expand-file-name "journal.org" org-directory))
  (setq org-capture-templates '(("T" "Immediate todo" entry
                                 (file +org-capture-todo-file)
                                 "* TODO %?\n%U\n%i")
                                ("j" "Journal" entry
                                 (file+olp+datetree +org-capture-journal-file)
                                 "* %U %?\n%i" :prepend t)))
  (setq org-archive-location "agenda_archive.org::")

  (setq org-use-fast-todo-selection 'expert)

  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-someday '((t (:inherit (bold font-lock-comment-face org-todo)))) "" ))
  (setq org-todo-keywords '((sequence "TEMP(e)" "TODO(t)" "PROJ(p)" "LOOP(l!)" "HABT(h!)" "WAIT(w@/@)" "PEND(n)" "IDEA(i)" "SOMEDAY(m)" "NOTE(o)" "|" "DONE(d!)" "KILL(k!)")))
  (setq org-todo-repeat-to-state t)
  (setq org-todo-keyword-faces '(("TODO" . org-todo)
                                 ("TEMP" . org-level-2)
                                 ("PROJ" . org-level-1)
                                 ("PEND" . org-level-3)
                                 ("LOOP" . +org-todo-active)
                                 ("HABT" . org-table)
                                 ("WAIT" . org-level-4)
                                 ("IDEA" . +org-todo-project)
                                 ("SOMEDAY" . +org-todo-someday)
                                 ("KILL" . org-agenda-dimmed-todo-face)
                                 ("NOTE" . org-agenda-dimmed-todo-face)))

  (defun my/time-grid-override (func list ndays todayp)
    "Show time grid items during scheduled blocks with org-scheduled face."
    (let* (
           ;; How frequent in minutes to have time grid intervals. This must match
           ;; the times in org-agenda-time-grid
           (time-grid-interval 30.0)
           (scheduled-times (mapcan #'(lambda (el)
                                        ;; only consider items with a scheduled time
                                        (if (and el (get-text-property 0 'time-of-day el))
                                            ;; get all scheduled items as pairs of (start time, end time)
                                            ;; end time is rounded to nearest time-grid-interval
                                            ;; all times are in minutes since midnight
                                            (let* ((time-num (get-text-property 0 'time-of-day el))
                                                   (duration (or (get-text-property 0 'duration el) 0))
                                                   (time-in-minutes (+ (* (/ time-num 100) 60) (mod time-num 100)))
                                                   (time-end (+ time-in-minutes duration))
                                                   (round-up #'(lambda (num)
                                                                 "Like round, but always round up from 0.5"
                                                                 (if (< (- (abs (- num (round num))) 0.5) 0.000001)
                                                                     (ceiling num)
                                                                   (round num))))
                                                   (time-end-rounded (* (funcall round-up (/ time-end time-grid-interval)) time-grid-interval)))
                                              (list (list time-in-minutes time-end-rounded)))))
                                    list))
           (additional (mapcan #'(lambda (time)
                                   (let ((time-in-minutes (+ (* (/ time 100) 60) (mod time 100))))
                                     ;; check if this time-grid item is near a scheduled item
                                     (if-let (cur-scheduled (cl-find-if
                                                             #'(lambda (scheduled)
                                                                 (let* ((sched-start (nth 0 scheduled))
                                                                        (sched-end (nth 1 scheduled)))
                                                                   ;; only show during scheduled time
                                                                   (and (> time-in-minutes sched-start)
                                                                        (< time-in-minutes sched-end))))
                                                             scheduled-times))
                                         ;; don't show this time-grid if it's the start of another scheduled item since the
                                         ;; scheduled item itself takes up a line
                                         (unless (cl-some #'(lambda (scheduled)
                                                              (let* ((sched-start (nth 0 scheduled))
                                                                     (sched-end (nth 1 scheduled)))
                                                                (= time-in-minutes sched-start)))
                                                          scheduled-times)
                                           (let* ((rawtimestr (replace-regexp-in-string " " "0" (format "%04s" time)))
                                                  (timestr (concat (substring rawtimestr 0 -2) ":" (substring rawtimestr -2)))
                                                  ;; show a different char for the last time-grid item for a particular scheduled item
                                                  (indicator-char (if-let ((end (nth 1 cur-scheduled))
                                                                           (end-diff (- end time-in-minutes))
                                                                           (diff-in-range (and (>= end-diff 0)
                                                                                               (<= end-diff time-grid-interval))))
                                                                      "┘"
                                                                    "│"))
                                                  (newel (org-agenda-format-item indicator-char (nth 3 org-agenda-time-grid)
                                                                                 nil "" nil timestr)))
                                             (put-text-property 2 (length newel) 'face 'org-scheduled newel)
                                             (list newel))))))
                               ;; needs to be the same text as time grid to get formatted correctly
                               (nth 1 org-agenda-time-grid)))
           (newlist (append additional list)))
      ;; call the original function (org-agenda-add-time-grid-maybe)
      (apply (cons func (list
                         ;; use the added list if we are using a time grid
                         (if org-agenda-use-time-grid newlist list)
                         ndays todayp)))))

  (advice-add 'org-agenda-add-time-grid-maybe :around #'my/time-grid-override)

  (define-advice org-insert-link (:around (fn &rest args) my/dwim-clipboard)
    (let ((clipboard-url (and (string-match-p "^http" (current-kill 0))
                              (current-kill 0))))
      (when clipboard-url (org-link--add-to-stored-links clipboard-url nil))
      (ignore-error quit (funcall-interactively fn args))
      (when clipboard-url (setq org-stored-links
                                (delq (assoc clipboard-url org-stored-links)
                                      org-stored-links)))))

  (add-hook 'org-mode-hook 'auto-revert-mode)

  ;; open pdfs in emacs (with pdf-tools)
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))

  (evil-add-command-properties #'org-up-element :jump t)
  (evil-add-command-properties #'org-agenda-goto :jump t)
  )

(use-package evil-nerd-commenter
  :demand t
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  :general
  ([remap comment-line] #'evilnc-comment-or-uncomment-lines)
  :config
  (evil-define-key 'motion 'global "gc" #'evilnc-comment-operator)
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
  (setq orderless-matching-styles '(orderless-literal
                                    orderless-regexp)
                                        ; not sure why this is needed, but gives error if this isn't set at all
        completion-lazy-hilit t)
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package moody
  :after evil
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
  :delight
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

  (defun my/presorted-completion-table (completions)
    "Return a completion table sorted by the order of the completions.

Requires lexical binding to be enabled."
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata (display-sort-function . ,#'identity))
        (complete-with-action action completions string pred))))

  (defun +beancount/clone-transaction (arg)
    "Clones a transaction to just beneath the current transaction.

Updates the date to the date of the previous transaction, unless at
the end of the buffer, in which case use today's date. Or, with a
prefix arg, updates the date to today.

Transactions must be separated by a blank line."
    (interactive "P")
    (let* ((start-date-regex "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
           (transaction-regex (concat start-date-regex " [!*] "))
           (use-today-date (or arg (eobp))))
      (save-restriction
        (widen)
        (when-let ((transaction
                    (completing-read
                     "Clone transaction: "
                     (my/presorted-completion-table (reverse (string-lines (buffer-string))))
                     (apply-partially #'string-match-p transaction-regex)
                     t)))
          (let* ((transaction
                  (save-excursion
                    (goto-char (point-min))
                    (re-search-forward (concat "^" (regexp-quote transaction)))
                    (buffer-substring-no-properties
                     (save-excursion
                       (beancount-goto-transaction-begin)
                       (re-search-forward " [!*] ")
                       (point))
                     (save-excursion
                       (beancount-goto-transaction-end)
                       (point)))))
                 (previous-date
                  (save-excursion
                    (forward-line)
                    (re-search-backward start-date-regex nil t)
                    (buffer-substring-no-properties
                     (point)
                     (re-search-forward start-date-regex))))
                 (today-date
                  (beancount--format-date (current-time)))
                 (date (if use-today-date
                           today-date
                         previous-date)))
            (re-search-forward "^$")
            (newline)
            (insert date " ! " transaction)
            ;; go to the amount on the last line of the transaction
            (forward-line -1)
            (re-search-forward "  " nil t)
            (re-search-forward "[0-9]" nil t)
            (backward-char))))))

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

  (defun +beancount/fava-stop ()
    "Stop the fava server."
    (interactive)
    (when beancount--fava-process
      (delete-process beancount--fava-process)
      (setq beancount--fava-process nil)
      (message "Fava process killed")))

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
   "mf" #'beancount-fava
   "mF" #'+beancount/fava-stop
   )
  )

(use-package marginalia
  :general-config
  (:keymaps 'minibuffer-local-map "M-a" #'marginalia-cycle)
  :config
  (setq marginalia--pangram "The quick brown fox jumps over the lazy dog")
  :init
  (marginalia-mode))

(use-package solaire-mode
  :demand t
  :config
  (solaire-global-mode +1))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :hook (prog-mode . copilot-mode)
  :hook (tex-mode . copilot-mode)
  :bind (("M-TAB" . 'copilot-accept-completion)
         ("M-<tab>" . 'copilot-accept-completion))

  :config
  (setq copilot-node-executable "/usr/local/bin/node")
  ;; to reduce memory use (can increase for debugging)
  (setq copilot-log-max 50)

  (defun my/copilot--get-source (orig-fun &rest args)
    "Advice to disable warnings"
    (let ((warning-minimum-level :emergency))
      (apply orig-fun args)))
  (advice-add #'copilot--get-source :around #'my/copilot--get-source)

  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  (add-to-list 'copilot-indentation-alist '(vimrc-mode 2))

  ;; Sometimes, the copilot-balancer-debug-buffer will be deleted and
  ;; the copilot-balancer--debug function will error. I override the
  ;; function and catch the "Selecting deleted buffer" error,
  ;; recreating the buffer if necessary
  (defun my/copilot-balancer--debug-wrapper (func &rest args)
    "Advice to catch error when debug buffer is deleted"
    (condition-case err
        (apply func args)
      (error
       (if (string= (cadr err) "Selecting deleted buffer")
           (progn
             (setq copilot-balancer-debug-buffer (get-buffer-create "*copilot-balancer-debug*"))
             (apply func args))
         (signal (car err) (cdr err))))))
  (advice-add #'copilot-balancer--debug :around #'my/copilot-balancer--debug-wrapper)
  )

(use-package magit)

(use-package vterm
  :after evil-collection
  :config
  ;; unbind C-q so we can use it as a prefix
  (evil-collection-define-key '(normal insert) 'vterm-mode-map
    (kbd "C-q") nil)

  (evil-collection-define-key 'insert 'vterm-mode-map
    (kbd "C-a") 'vterm--self-insert
    (kbd "C-b") 'vterm--self-insert
    (kbd "C-d") 'vterm--self-insert
    (kbd "C-e") 'vterm--self-insert
    (kbd "C-f") 'vterm--self-insert
    (kbd "C-k") 'vterm--self-insert
    (kbd "C-l") 'vterm--self-insert
    (kbd "C-n") 'vterm--self-insert
    (kbd "C-o") 'vterm--self-insert
    (kbd "C-p") 'vterm--self-insert
    ;; no C-q since it's used as a prefix key
    (kbd "C-r") 'vterm--self-insert
    (kbd "C-s") 'vterm--self-insert
    (kbd "C-t") 'vterm--self-insert
    (kbd "C-u") 'vterm--self-insert
    (kbd "C-v") 'vterm--self-insert
    (kbd "C-w") 'vterm--self-insert
    (kbd "C-x") 'vterm--self-insert
    (kbd "C-y") 'vterm--self-insert
    ;; no C-z since it's used as a prefix key
    (kbd "C-6") #'(lambda () (interactive) (vterm-send (kbd "C-^")))
    (kbd "<delete>") 'vterm-send-delete)

  (evil-collection-define-key '(normal insert) 'vterm-mode-map
    (kbd "C-z") #'vterm-send-next-key ; vterm "leader" (to send all ctrl keys)
    (kbd "C-q C-t") #'vterm-copy-mode
    (kbd "C-q C-z") #'evil-collection-vterm-toggle-send-escape
    (kbd "C-q C-q") #'evil-normal-state
    (kbd "C-q C-l") #'vterm-clear-scrollback
    ;; send C-g to vterm
    (kbd "C-q g") #'(lambda () (interactive) (vterm-send "C-g")))

  ;; pass through select ctrl- keys
  (evil-collection-define-key 'normal 'vterm-mode-map
    (kbd "C-c") #'vterm--self-insert
    (kbd "C-p") #'vterm--self-insert
    (kbd "C-n") #'vterm--self-insert)
  (evil-collection-define-key 'insert 'vterm-mode-map
    (kbd "C-c") #'vterm--self-insert
    (kbd "M-<left>") #'vterm-send-M-b
    (kbd "M-<right>") #'vterm-send-M-f)

  (setq vterm-eval-cmds (append vterm-eval-cmds
                                '(("evil-emacs-state" evil-emacs-state)
                                  ("evil-insert-state" evil-insert-state)
                                  ("evil-collection-vterm-toggle-send-escape" evil-collection-vterm-toggle-send-escape))))

  (setq vterm-environment '("LC_INSIDE_EMACS=vterm"))
  (setq vterm-buffer-name-string "*vterm* %s")

  ;; counterintuitively, this actually stops the cursor from moving back when set to t
  (setq evil-collection-vterm-move-cursor-back t)

  ;; add indicator to mode line only for vterm mode that shows value
  ;; of evil-collection-vterm-send-escape-to-vterm-p
  (defun my/vterm-send-escape-indicator ()
    (if (and (eq major-mode 'vterm-mode)
             evil-collection-vterm-send-escape-to-vterm-p)
        " <esc>"))
  (add-to-list 'mode-line-position
               '(:eval (my/vterm-send-escape-indicator))
               :append)

  ;; from here: https://github.com/akermu/emacs-libvterm/issues/569#issuecomment-2244574273
  ;; fixes the issue where tramp messes up tty on remote shells
  (define-advice vterm--get-shell (:filter-return (vterm-shell) quote-remote)
    "Quote VTERM-SHELL if it's a remote shell.

Fixes the issue where tramp messes up tty on remote shells. By quoting
the shell, this prevents tramp from enabling heredoc mode and changing
fd 0 to something different than fd 1 and 2."
    (if (and (ignore-errors (file-remote-p default-directory))
             (not (string-match-p "'.*'" vterm-shell)))
        (format "'%s'" vterm-shell)
      vterm-shell))

  )

(use-package evil-numbers
  :demand t
  :config
  (leader-def
    "+" #'evil-numbers/inc-at-pt
    "-" #'evil-numbers/dec-at-pt))

(use-package yaml-mode)

(use-package dockerfile-mode)

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :hook ((pdf-view-mode . auto-revert-mode)
         ;; hide cursor
         (pdf-view-mode . (lambda () (blink-cursor-mode -1)))
         ;; isearch is kinda useless in PDFs imo
         (evil-collection-setup . (lambda (&rest _)
                                    ;; (general-def 'pdf-view-mode-map "gr" #'revert-buffer-quick)
                                    (general-def 'normal 'pdf-view-mode-map "/" #'pdf-occur))))
  ;; :general-config
  ;; ('normal 'pdf-view-mode-map "J" #'pdf-view-scroll-down-or-previous-page)
  ;; ('normal 'pdf-view-mode-map "K" #'pdf-view-scroll-up-or-next-page)
  :config
  (add-to-list 'debug-ignored-errors "\\`No such page:")
  (setq pdf-view-use-scaling t)
  (setq-default pdf-view-display-size 'fit-page))

(use-package vimrc-mode)

(use-package gptel
  :config

  (setq gptel-directives '((default . "You are a large language model and a helpful assistant. Respond concisely.")
                           (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
                           (writing . "You are a large language model and a writing assistant. Respond concisely.")
                           (chat . "You are a large language model and a conversation partner. Respond concisely.")))
  (setq gptel-model 'deepseek/deepseek-r1-distill-llama-70b:free)
  (setq gptel-backend
        ;; OpenRouter offers an OpenAI compatible API
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key (with-temp-buffer
                 (insert-file-contents (expand-file-name "~/.openrouter_key"))
                 (buffer-string))
          :models '(deepseek/deepseek-r1-distill-llama-70b:free
                    google/gemini-2.0-pro-exp-02-05:free))))

(straight-use-package
 '(ultra-scroll
   :type git
   :host github
   :repo "jdtsmith/ultra-scroll"))

(use-package ultra-scroll
  :demand t
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(use-package csv-mode)

(use-package rust-mode)
(use-package rustic
  :init
  (setq rustic-lsp-setup-p nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" default)))
