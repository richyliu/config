;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq initial-frame-alist
      (append initial-frame-alist
              '((width . 200)
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
(setq org-directory "~/org/")


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

;; use ctrl-tab to accept copilot completion
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion)
         ("C-<tab>" . 'copilot-accept-completion)))
(after! copilot
  (setq copilot-node-executable "/usr/local/bin/node16")
  ;; to reduce memory use; can increase for debugging
  (setq copilot-log-max 50))

(use-package! elcord)
(after! elcord
  (setq elcord-editor-icon "emacs_icon")
  (setq elcord-quiet t)
  (elcord-mode))

(after! irony
  (setq irony-disable-over-tramp t))

(after! nov
  ;; use nov for epub
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-text-width 80)
  ;; change font
  (defun my-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch
                             :family "Liberation Serif"
                             :height 1.2))
  (add-hook 'nov-mode-hook 'my-nov-font-setup))

(after! vterm
  ;; fix shells
  (setq vterm-tramp-shells '(("ssh" "/bin/zsh")))
  (setq vterm-environment '("TMUX=none"))
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
  ;; make vterm keymaps more usage
  ;; we only want these to take effect inside vterm buffers
  (defun my-vterm-keymap-override-setup ()
    ;; vterm map "leader" (to send all ctrl keys)
    (define-key evil-normal-state-local-map (kbd "C-o") vterm-mode-map)
    (define-key evil-insert-state-local-map (kbd "C-o") vterm-mode-map)
    ;; use C-c to send actual C-c
    (define-key evil-normal-state-local-map (kbd "C-c") #'vterm-send-C-c)
    (define-key evil-insert-state-local-map (kbd "C-c") #'vterm-send-C-c)
    ;; copy mode
    (define-key evil-normal-state-local-map (kbd "C-x c") #'vterm-copy-mode)
    (define-key evil-insert-state-local-map (kbd "C-x c") #'vterm-copy-mode))
  (add-hook 'vterm-mode-hook #'my-vterm-keymap-override-setup))


;;; Other package settings

(when (boundp 'agda2-mode)
  ;; we also need doom emacs's adga for the keybindings
  ;; enable agda for markdown files
  (add-to-list 'auto-mode-alist '("\\.lagda.md\\'" . agda2-mode)))

;; add qemu include path for flycheck
(add-hook 'c-mode-hook
          (lambda () (setq flycheck-clang-include-path
                           (list (expand-file-name "~/code/neojetset-qemu/include")
                                 (expand-file-name "~/code/neojetset-qemu/build")
                                 ))))


;;; Keybindings

(map!
 ;; use meta-number (alt-number) to jump to tab
 (:when (featurep! :ui tabs)
  :g "M-1" nil
  :g "M-2" nil
  :g "M-3" nil
  :g "M-4" nil
  :g "M-5" nil
  :g "M-6" nil
  :g "M-7" nil
  :g "M-8" nil
  :g "M-9" nil
  :n "M-1" (lambda () (interactive) (+tabs:next-or-goto 1))
  :n "M-2" (lambda () (interactive) (+tabs:next-or-goto 2))
  :n "M-3" (lambda () (interactive) (+tabs:next-or-goto 3))
  :n "M-4" (lambda () (interactive) (+tabs:next-or-goto 4))
  :n "M-5" (lambda () (interactive) (+tabs:next-or-goto 5))
  :n "M-6" (lambda () (interactive) (+tabs:next-or-goto 6))
  :n "M-7" (lambda () (interactive) (+tabs:next-or-goto 7))
  :n "M-8" (lambda () (interactive) (+tabs:next-or-goto 8))
  :n "M-9" (lambda () (interactive) (+tabs:next-or-goto 9)))

 ;; cmd-w to kill buffer instead of workspace
 :g "s-w" #'kill-current-buffer

 ;; disable evil-lion bindings that conflict with org mode
 :n "gl" nil

 ;; disable aya-expand keymap (conflicts with copilot completion)
 :i "C-<tab>" nil

 (:when (featurep! :term vterm)
  (:leader
   :desc "Open projectile vterm" "p v" #'projectile-run-vterm
   :desc "Open vterm buffer" "b v" #'vterm))

 (:leader
  :desc "Kill all buffers" "q a" #'(lambda () (interactive)
                                     (mapc #'kill-buffer (buffer-list))))

 (:map minibuffer-local-map
  ;; go to normal mode with C-f (like command line edit mode)
  "C-f" #'evil-normal-state))


;;; General emacs settings

(setq which-key-idle-delay 1.0)

(setq delete-by-moving-to-trash nil)

;; group tabs by project
(defun my--projectile-groups ()
  ;; only use default group ("-") if it's a *star buffer* but not vterm or eshell
  ;; (we want to keep terminal buffers with its respective project)
  (if-let ((buf-name (buffer-name))
           (buf-name-first (substring buf-name 0 1))
           (star-buffer-p (string-equal buf-name-first "*"))
           (not-vterm-p (not (or (string-match-p "vterm" buf-name)
                                 (string-match-p "eshell" buf-name)))))
      (list "-")
    (if-let ((projectile-name (projectile-project-name)))
        (list projectile-name)
      (if-let ((doom-proj-name (doom-project-name)))
          doom-proj-name
        (list "default")))))
(setq centaur-tabs-buffer-groups-function #'my--projectile-groups)


;;; Mode lists

;; set C mode for .cpc files
(add-to-list 'auto-mode-alist '("\\.cpc\\'" . c-mode))


;;; Hooks

(defun my--set-shift-2 ()
  (setq evil-shift-width 2))
(add-hook 'html-mode-hook #'my--set-shift-2)
(add-hook 'css-mode-hook #'my--set-shift-2)
(add-hook 'js-mode-hook #'my--set-shift-2)

;; enable folding in text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)


;;; Advice

;; always cache man-completion-table (even across calls) for faster speed on mac
(defvar my--Man-cache nil)
(defun my--Man-completion-always-cache (_string _pred _action)
  (if Man-completion-cache
      (setq my--Man-cache Man-completion-cache)
    (setq Man-completion-cache my--Man-cache)))
(advice-add 'Man-completion-table :before #'my--Man-completion-always-cache)

;; fix yas snippet edit
;; caused by vertico stripping text properties when completing
;; copied fix from: https://github.com/doomemacs/doomemacs/issues/4127#issuecomment-1019731798
(defun my--yas-snippet--completing-read-uuid (prompt all-snippets &rest args)
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
(advice-add '+snippet--completing-read-uuid :override #'my--yas-snippet--completing-read-uuid)
