;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq initial-frame-alist
      (append initial-frame-alist
              '((width . 145)
                (height . 70))))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(setq doom-font (font-spec
                 :family "iosevka ss07"
                 :width 'expanded
                 :size 15
                 ))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "/Users/richard/Documents/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;;; Package settings

(after! circe
  (setq circe-network-options
        '(("OFTC"
           :tls t
           :nick "richyliu2"
           :channels ("#qemu-gsoc"))
          ("Libera Chat"
           :tls t
           :nick "richyliu2"
           :channels ("#emacs" "#emacs-beginners" "#emacs-til")))))

(defun my/projectile-groups ()
  "Group tabs by projectile project."
  (cond
   ;; group org-agenda-mode buffers with org mode
   ((string-equal major-mode "org-agenda-mode")
    (list org-directory))
   ;; use default group ("-") for vterm shells
   ((string-equal major-mode "vterm-mode")
    '("-"))
   ;; use default group ("-") for *star* buffers
   ((string-equal (substring (buffer-name) 0 1) "*")
    '("-"))
   ;; hide certain org buffers
   ((and (string-equal (projectile-project-root) org-directory)
         (or (member (buffer-name) '("inbox.org" "notes.org" "journal.org"))
             (string-match-p "_archive$" (buffer-name))))
    (list (concat org-directory "--hidden")))
   ;; otherwise use projectile root (to separate projects with same name)
   (t
    (list (projectile-project-root)))))
(after! centaur-tabs
  (setq centaur-tabs-buffer-groups-function #'my/projectile-groups))
(centaur-tabs-mode 1)


;; use ctrl-tab to accept copilot completion
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("M-TAB" . 'copilot-accept-completion)
         ("M-<tab>" . 'copilot-accept-completion)))
(after! copilot
  (setq
   copilot-node-executable "/usr/local/bin/node16"
   ;; to reduce memory use; can increase for debugging
   copilot-log-max 50))

(use-package! elcord)
(after! elcord
  (setq
   elcord-editor-icon "emacs_icon"
   elcord-quiet t)
  (elcord-mode))

(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

(after! flycheck
  ;; add qemu include path for flycheck
  (add-hook 'c-mode-hook
            (lambda () (setq flycheck-clang-include-path
                             (list (expand-file-name "~/code/neojetset-qemu/include")
                                   (expand-file-name "~/code/neojetset-qemu/build")
                                   )))))

;; override irony-mode to enable only for non-TRAMP files
(defun my/disable-irony-mode-if-remote (oldfun &rest args)
  "Disable irony-mode if the current buffer is on a remote host."
  (unless (and buffer-file-name (file-remote-p buffer-file-name))
    (apply oldfun args)))
(after! irony
  (advice-add #'irony-mode :around #'my/disable-irony-mode-if-remote))


(after! nov
  ;; use nov for epub
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-text-width 80)
  ;; change font
  (defun my/nov-font-setup ()
    (face-remap-add-relative 'variable-pitch
                             :family "Liberation Serif"
                             :height 1.2))
  (add-hook 'nov-mode-hook #'my/nov-font-setup))

(defun my/org-mode-hook ()
  (setq-local
   company-idle-delay nil
   ;; use visual line numbers for folded org-mode
   display-line-numbers 'visual)
  (map!
   ;; go to beginning of line (not including bullets) in org
   :m "^" #'org-beginning-of-line
   :n "C-j" #'org-next-visible-heading
   :n "C-k" #'org-previous-visible-heading))
(after! org
  (with-no-warnings
    (custom-declare-face '+org-todo-maybe '((t (:inherit (bold font-lock-comment-face org-todo)))) ""))
  (setq
   org-agenda-files '("inbox.org" "agenda.org")
   org-priority-default ?C
   org-priority-start-cycle-with-default nil
   org-log-into-drawer t
   org-todo-keywords '((sequence "LOOP(r)" "EVENT(e)" "TODO(t)" "NEXT(n)" "IDEA(i)" "MAYBE(m)" "|" "DONE(d@)")
                       (sequence "[ ](T)" "|" "[X](D)"))
   org-todo-keyword-faces
   '(("[-]"  . +org-todo-active)
     ("STRT" . +org-todo-active)
     ("[?]"  . +org-todo-onhold)
     ("NEXT" . +org-todo-onhold)
     ("IDEA" . +org-todo-project)
     ("MAYBE" . +org-todo-maybe)
     ("KILL" . +org-todo-cancel))
   org-agenda-sorting-strategy '((agenda time-up category-keep scheduled-up priority-down todo-state-up)
                                 (todo category-keep todo-state-up scheduled-up priority-down)
                                 (tags category-keep scheduled-up priority-down todo-state-up)
                                 (search category-keep))
   +org-capture-todo-file "inbox.org"
   org-capture-templates '(("t" "Personal todo" entry
                            (file +org-capture-todo-file)
                            "* TODO %?\n%T\n%i\n%a")
                           ("n" "Personal notes" entry
                            (file+headline +org-capture-notes-file "Inbox")
                            "* %u %?\n%i\n%a" :prepend t)
                           ("j" "Journal" entry
                            (file+olp+datetree +org-capture-journal-file)
                            "* %U %?\n%i\n%a" :prepend t)
                           ("p" "Templates for projects")
                           ("pt" "Project-local todo" entry
                            (file+headline +org-capture-project-todo-file "Inbox")
                            "* TODO %?\n%i\n%a" :prepend t)
                           ("pn" "Project-local notes" entry
                            (file+headline +org-capture-project-notes-file "Inbox")
                            "* %U %?\n%i\n%a" :prepend t)
                           ("pc" "Project-local changelog" entry
                            (file+headline +org-capture-project-changelog-file "Unreleased")
                            "* %U %?\n%i\n%a" :prepend t)
                           ("o" "Centralized templates for projects")
                           ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
                           ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
                           ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)))
  (add-hook 'org-mode-hook #'my/org-mode-hook)
  ;; flash the cursor after an org agenda jump to file
  (advice-add 'org-agenda-switch-to :after #'+nav-flash/blink-cursor)
  (advice-add 'org-agenda-goto :after #'+nav-flash/blink-cursor))

(defun my/vterm-keymap-override-setup ()
  "Make vterm keymaps more usable."
  ;; vterm map "leader" (to send all ctrl keys)
  (define-key evil-normal-state-local-map (kbd "C-o") vterm-mode-map)
  (define-key evil-insert-state-local-map (kbd "C-o") vterm-mode-map)
  ;; use C-c to send actual C-c
  (define-key evil-normal-state-local-map (kbd "C-c") #'vterm-send-C-c)
  (define-key evil-insert-state-local-map (kbd "C-c") #'vterm-send-C-c)
  ;; copy mode
  (define-key evil-normal-state-local-map (kbd "C-x c") #'vterm-copy-mode)
  (define-key evil-insert-state-local-map (kbd "C-x c") #'vterm-copy-mode)
  ;; toggle send esc
  (define-key evil-normal-state-local-map (kbd "C-x z") #'evil-collection-vterm-toggle-send-escape)
  (define-key evil-insert-state-local-map (kbd "C-x z") #'evil-collection-vterm-toggle-send-escape)
  ;; clear scrollback
  (define-key evil-normal-state-local-map (kbd "C-x l") #'vterm-clear-scrollback)
  (define-key evil-insert-state-local-map (kbd "C-x l") #'vterm-clear-scrollback)
  ;; send ctrl-p/n to vterm directly
  (define-key evil-normal-state-local-map (kbd "C-p") #'vterm-send-C-p)
  (define-key evil-normal-state-local-map (kbd "C-n") #'vterm-send-C-n))
(after! vterm
  ;; fix shells
  (setq vterm-tramp-shells '(("ssh" "/bin/zsh")))
  (setq vterm-environment '("TMUX=none"))
  (setq vterm-kill-buffer-on-exit nil)
  (map!
   (:map vterm-mode-map
    ;; alt-backspace to delete word in vterm insert mode
    :i "M-<backspace>" #'vterm-send-meta-backspace
    ;; originally behind C-c, move them to vterm map
    "C-g" #'vterm-send-C-g
    "C-u" #'vterm-send-C-u
    ;; sent C-l by default, also can send vterm-clear
    "C-l" #'vterm-send-C-l
    "C-c l" #'vterm-clear
    ;; missing in original vterm-mode-map
    "C-x" #'vterm-send-C-x
    "C-y" #'vterm-send-C-y
    ))
  (add-hook 'vterm-mode-hook #'my/vterm-keymap-override-setup))

(defun my/+snippet--completing-read-uuid (prompt all-snippets &rest args)
    " Fix `+snippets/edit' error caused by vertico stripping text properties when completing.

Overrides `+snippet--completing-read-uuid' to strip text properties.
Copied fix from: https://github.com/doomemacs/doomemacs/issues/4127#issuecomment-1019731798"
    (let* ((snippet-data (cl-loop for (_ . tpl) in (mapcan #'yas--table-templates (if all-snippets
                                                                                      (hash-table-values yas--tables)
                                                                                    (yas--get-snippet-tables)))
                                  for txt = (format "%-25s%-30s%s"
                                                    (yas--template-key tpl)
                                                    (yas--template-name tpl)
                                                    (abbreviate-file-name (yas--template-load-file tpl)))
                                  collect
                                  `(,txt . ,(yas--template-uuid tpl))))
           (selected-value (apply #'completing-read prompt snippet-data args)))
      (alist-get selected-value snippet-data nil nil 'equal)))
(after! yasnippet
  (advice-add '+snippet--completing-read-uuid :override #'my/+snippet--completing-read-uuid))

;;; Keybindings

(defun my/reset-doom ()
  "Kill all buffers in buffer-list and cd back to home"
  (interactive)
  (mapc #'kill-buffer (buffer-list))
  (cd "~/")
  (delete-other-windows))

(defmacro my/goto-tab-n (n)
  `(lambda ()
     "Goto tab N"
     (interactive)
     (+tabs:next-or-goto ,n)))

(map!
 (:when (modulep! :ui tabs)
  ;; use meta-number (alt-number) to jump to tab
  :g "M-1" (my/goto-tab-n 1)
  :g "M-2" (my/goto-tab-n 2)
  :g "M-3" (my/goto-tab-n 3)
  :g "M-4" (my/goto-tab-n 4)
  :g "M-5" (my/goto-tab-n 5)
  :g "M-6" (my/goto-tab-n 6)
  :g "M-7" (my/goto-tab-n 7)
  :g "M-8" (my/goto-tab-n 8)
  :g "M-9" (my/goto-tab-n 9)

  ;; use SPC-number to jump to tab
  (:leader
   :desc "Buffer tab 1" :n "1" (my/goto-tab-n 1)
   :desc "Buffer tab 2" :n "2" (my/goto-tab-n 2)
   :desc "Buffer tab 3" :n "3" (my/goto-tab-n 3)
   :desc "Buffer tab 4" :n "4" (my/goto-tab-n 4)
   :desc "Buffer tab 5" :n "5" (my/goto-tab-n 5)
   :desc "Buffer tab 6" :n "6" (my/goto-tab-n 6)
   :desc "Buffer tab 7" :n "7" (my/goto-tab-n 7)
   :desc "Buffer tab 8" :n "8" (my/goto-tab-n 8)
   :desc "Buffer tab 9" :n "9" (my/goto-tab-n 9)))

 (:when (modulep! :ui workspaces)
  :g "s-1" #'+workspace/switch-to-0
  :g "s-2" #'+workspace/switch-to-1
  :g "s-3" #'+workspace/switch-to-2
  :g "s-4" #'+workspace/switch-to-3
  :g "s-5" #'+workspace/switch-to-4
  :g "s-6" #'+workspace/switch-to-5
  :g "s-7" #'+workspace/switch-to-6
  :g "s-8" #'+workspace/switch-to-7
  :g "s-9" #'+workspace/switch-to-final)


 ;; cmd-shift-[/] to switch workspace
 :g "s-{" #'+workspace/switch-left
 :g "s-}" #'+workspace/switch-right

 ;; cmd-w to kill buffer instead of workspace
 :g "s-w" #'kill-current-buffer
 ;; cmd-d to kill workspace
 :g "s-d" #'+workspace/delete

 (:mode org-mode
  ;; cmd-k to link in org mode
  :g "s-k" #'org-insert-link)

 ;; disable evil-lion bindings that conflict with org mode
 :n "gl" nil

 ;; disable aya-expand keymap (conflicts with copilot completion)
 :i "C-<tab>" nil

 (:leader
  :desc "Kill all buffers" "q a" #'my/reset-doom
  :desc "Sync org with remote" "n r" (lambda ()
                                       (interactive)
                                       (call-process (concat org-directory "beorg_sync.sh"))
                                       (message "Synced org with remote"))

  (:when (modulep! :ui nav-flash)
   :desc "Blink current line" "b L" #'+nav-flash/blink-cursor)

  (:when (modulep! :term vterm)
   :desc "Open projectile vterm" "p v" #'projectile-run-vterm
   :desc "Open vterm buffer" "b v" #'vterm))

 (:map evil-window-map
  ;; unmap SPC w C-h so it can run help instead
  "C-h" nil)

 (:map minibuffer-local-map
  ;; go to normal mode with C-f (like command line edit mode)
  "C-f" #'evil-normal-state)

 ;; make { and } (paragraph motions) work linewise
 :o "}" #'(lambda ()
            (interactive)
            (evil-visual-line)
            (evil-forward-paragraph)
            (evil-visual-line))
 :o "{" #'(lambda ()
            (interactive)
            (evil-visual-line)
            (evil-backward-paragraph)
            (evil-visual-line)))


;;; General emacs settings

(setq
 delete-by-moving-to-trash nil
 evil-emacs-state-cursor '("red" bar))

;; set C mode for .cpc files
(add-to-list 'auto-mode-alist '("\\.cpc\\'" . c-mode))


(defun my/set-shift-2 ()
  (setq evil-shift-width 2))
(add-hook 'html-mode-hook #'my/set-shift-2)
(add-hook 'css-mode-hook #'my/set-shift-2)
(add-hook 'js-mode-hook #'my/set-shift-2)

;; enable folding in text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)


(defvar my/Man-cache nil
  "Cache variable used for `my/Man-completion-always-cache'")
(defun my/Man-completion-always-cache (_string _pred _action)
  "Always cache the `Man-completion-cache' (even across calls) for faster speed on mac."
  (if Man-completion-cache
      (setq my/Man-cache Man-completion-cache)
    (setq Man-completion-cache my/Man-cache)))
(advice-add 'Man-completion-table :before #'my/Man-completion-always-cache)
