; hide GUI elements on startup
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

; disable package.el because we are using straight.el
(setq package-enable-at-startup nil)

; defer for lazy loading
(setq use-package-always-defer t)

(setq initial-frame-alist
      (append initial-frame-alist
              '((width . 120)
                (height . 50))))

(setq gc-cons-threshold most-positive-fixnum)
