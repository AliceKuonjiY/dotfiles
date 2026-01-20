;; Load local themes
(add-to-list 'custom-theme-load-path
             "~/.emacs.d/themes")

(mapc #'disable-theme custom-enabled-themes)  ; Disable all active themes
(setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, or 'mocha
(load-theme 'doom-moonlight t)  ; Load the built-in theme
;;(load-theme 'gruber-darker t)

(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq-default confirm-nonexistent-file-or-buffer nil)

;; Set font
(set-face-attribute 'default nil
                    :height 140 :weight 'medium :family "Iosevka")
(set-fontset-font
    (frame-parameter nil 'font)
    'han
    (font-spec :name "Sarasa Mono SC"))
(set-fontset-font
    (frame-parameter nil 'font)
    'cjk-misc
    (font-spec :name "Sarasa Mono SC"))

;; In Emacs, customization variables modified via the UI (e.g., M-x customize)
;; are typically stored in a separate file, commonly named 'custom.el'. To
;; ensure these settings are loaded during Emacs initialization, it is necessary
;; to explicitly load this file if it exists.
(load custom-file 'noerror 'no-message)

;; Allow Emacs to upgrade built-in packages, such as Org mode
(setq package-install-upgrade-built-in t)

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

;; Remove title bar
(push '(undecorated . t) default-frame-alist)

;; Auto insert matching brackets and quotes
(electric-pair-mode t)

;; Set indent
(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq-default c-basic-offset 4)
