;;; my-config.el --- My config -*- lexical-binding:t -*-

(setq url-proxy-services
      '(("http" . "127.0.0.1:7897")
        ("https" . "127.0.0.1:7897")))

;; theme
(load-theme 'doom-gruvbox t)

;; (load-theme 'catppuccin t)
;; (setq catppuccin-flavor 'mocha) ;; 'frappe or 'latte, 'macchiato, or 'mocha

(set-face-attribute 'font-lock-comment-face   nil               :slant 'italic)
(set-face-attribute 'font-lock-builtin-face   nil :weight 'bold               )
(set-face-attribute 'font-lock-keyword-face   nil :weight 'bold :slant 'italic)
(set-face-attribute 'font-lock-type-face      nil :weight 'bold               )
(set-face-attribute 'line-number-current-line nil :weight 'bold               )

;; Set font
(set-face-attribute 'default nil :height 140 :weight 'regular :family "IosevkaCustom")
(set-fontset-font (frame-parameter nil 'font) 'han (font-spec :name "Sarasa Mono SC"))
(set-fontset-font (frame-parameter nil 'font) 'cjk-misc (font-spec :name "Sarasa Mono SC"))

;; In Emacs, customization variables modified via the UI (e.g., M-x customize)
;; are typically stored in a separate file, commonly named 'custom.el'. To
;; ensure these settings are loaded during Emacs initialization, it is necessary
;; to explicitly load this file if it exists.
(load custom-file 'noerror 'no-message)

;; Allow Emacs to upgrade built-in packages, such as Org mode
(setq package-install-upgrade-built-in t)

(setq mouse-wheel-progressive-speed nil)

(setq confirm-kill-emacs #'y-or-n-p)

;; When Delete Selection mode is enabled, typed text replaces the selection
;; if the selection is active.
(delete-selection-mode 1)

;; Display the current line and column numbers in the mode line
(setq line-number-mode t)
(setq column-number-mode t)
(setq mode-line-position-column-line-format '("%l:%C"))

;; Display of line numbers in the buffer:
(setq-default display-line-numbers-type 'relative)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'display-line-numbers-mode))

;; Paren match highlighting
(add-hook 'after-init-hook #'show-paren-mode)

;; Window dividers separate windows visually. Window dividers are bars that can
;; be dragged with the mouse, thus allowing you to easily resize adjacent
;; windows.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Dividers.html
(add-hook 'after-init-hook #'window-divider-mode)

;; Display time
(setq display-time-24hr-format t)
(display-time-mode t)

;; Display size indication
(size-indication-mode 1)

;; Remove title bar
(if (eq window-system 'pgtk)
    (push '(undecorated . t) default-frame-alist))

;; Auto insert matching brackets and quotes
(electric-pair-mode t)

;; inhibit auto completion
(setq completion-auto-help nil)

;; Set indent
(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq c-ts-mode-indent-offset 4)

;; Auto move cursor to compilation buffer
(setq compilation-scroll-output 'first-error)

;; Whitespace mode
(setq whitespace-style '(face spaces newline tabs trailing
                              space-mark tab-mark newline-mark))

;;; my-config.el ends here
