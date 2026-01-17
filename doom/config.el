;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; modify /usr/share/X11/xkb/keycodes/evdev to change CapsLock to Ctrl

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
        ("http" . "127.0.0.1:7897")
        ("https" . "127.0.0.1:7897")))

;;; Custom functions
(defun yhu/next-5-lines ()
  "Move the cursor down by 5 lines."
  (interactive)
  (dotimes (_ 5) (next-line)))

(defun yhu/previous-5-lines ()
  "Move the cursor up by 5 lines."
  (interactive)
  (dotimes (_ 5) (previous-line)))

(defun yhu/read-lines-to-list (file-path)
  "Read the contents of FILE-PATH and return a list of lines."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun yhu/shrink-window-horizontally (n)
  "Shrink the window horizontally by N columns."
  (interactive "nNumber of columns to shrink: ")
  (shrink-window-horizontally (n) t))

(defun yhu/enlarge-window-horizontally (n)
  "Enlarge the window horizontally by N columns."
  (interactive "nNumber of columns to enlarge: ")
  (enlarge-window-horizontally n t))

(defun yhu/shrink-window (n)
  "Shrink the window vertically by N lines."
  (interactive "nNumber of lines to shrink: ")
  (shrink-window n))

(defun yhu/enlarge-window (n)
  "Enlarge the window vertically by N lines."
  (interactive "nNumber of lines to enlarge: ")
  (enlarge-window n))

(defun yhu/create-anime-banner ()
  (let* ((banner (yhu/read-lines-to-list "~/.config/doom/ascii-pic/emacs.txt"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(defun yhu/backward-kill-word ()
  "Remove all whitespace if the character behind the cursor is whitespace, otherwise remove a word."
  (interactive)
  (if (looking-back "[^a-zA-Z0-9\n]")
      ;; delete horizontal space before us and then check to see if we
      ;; are looking at a newline
      (progn (delete-horizontal-space 't)
             (while (looking-back "[^a-zA-Z0-9\n]")
               (backward-delete-char 1)))
    ;; otherwise, just do the normal kill word.
    (if (looking-back "[a-zA-Z0-9]")
        (backward-kill-word 1)
      (backward-delete-char 1))))


;;; General settings
;; (setq debug-on-error t)
(whole-line-or-region-global-mode)
(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq-default c-basic-offset 4)
(setq-default confirm-kill-emacs nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(push '(undecorated . t) default-frame-alist)
(setq display-time-default-load-average nil)
(setq display-time-24hr-format t)
(display-time-mode t)


;;; Custom keybindings
(global-set-key (kbd "C-M-=") 'eval-print-last-sexp)
(global-set-key (kbd "C-z") 'doom/escape)
(global-set-key (kbd "C-<") 'yhu/previous-5-lines)
(global-set-key (kbd "C->") 'yhu/next-5-lines)
(global-set-key (kbd "C-.") 'er/expand-region)
(global-set-key (kbd "C-,") 'er/contract-region)
(global-set-key (kbd "C-<backspace>") 'yhu/backward-kill-word)
(global-set-key (kbd "C-c t t") 'whitespace-mode)


;;; Define hydra for text scaling
(defhydra hydra-text-scale (global-map "C-c t s")
  "scale text"
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" (text-scale-set 0) "reset")
  ("q" nil "finished" :exit t))

(defhydra hydra-window-resize (global-map "C-x w r")
  "resize window"
  ("<left>" (yhu/shrink-window-horizontally 5) "shrink left")
  ("<right>" enlarge-window-horizontally "enlarge right")
  ("<down>" shrink-window "shrink down")
  ("<up>" enlarge-window "enlarge up")
  ("q" nil "finished" :exit t))

(defhydra hydra-window-move (global-map "C-x w m")
  "move window"
  ("<left>" windmove-left "move left")
  ("<right>" windmove-right "move right")
  ("<up>" windmove-up "move up")
  ("<down>" windmove-down "move down")
  ("q" nil "finished" :exit t))

(defhydra hydra-switch-buffer (global-map "C-c b")
  "switch buffer"
  ("n" next-buffer "next")
  ("p" previous-buffer "previous")
  ("q" nil "finished" :exit t))

(defhydra hydra-move-cursor (global-map "C-c C-m")
  "move cursor"
  ("h" backward-char "left")
  ("l" forward-char "right")
  ("k" previous-line "up")
  ("j" next-line "down")
  ("H" beginning-of-line "beginning")
  ("L" end-of-line "end")
  ("n" forward-word "next word")
  ("p" backward-word "previous word")
  ("q" nil "finished" :exit t))

(defhydra hydra-multiple-cursors (global-map "C-c m c")
  "multiple cursors"
  ("l" mc/edit-lines "edit line" :exit t)
  ("a" mc/mark-all-like-this "mark all" :exit t)
  ("n" mc/mark-next-like-this "mark next")
  ("N" mc/skip-to-next-like-this "skip next")
  ("M-n" mc/unmark-next-like-this "unmark next")
  ("p" mc/mark-previous-like-this "mark previous")
  ("P" mc/skip-to-previous-like-this "skip previous")
  ("M-p" mc/unmark-previous-like-this "unmark previous")
  ("|" mc/vertical-align "align with spaces")
  ("s" mc/mark-all-in-region-regexp "mark all in region" :exit t)
  ("0" mc/insert-numbers "insert numbers" :exit t)
  ("A" mc/insert-letters "insert letters" :exit t)
  ("<mouse-1>" mc/add-cursor-on-click "add cursor")
  ;; Help with click recognition in this hydra
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore)
  ("q" nil "quit"))

;;; Plugin configurations
(setq +doom-dashboard-ascii-banner-fn #'yhu/create-anime-banner)

(use-package! ivy
  :defer t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-s" . 'swiper)))

(use-package! smooth-scrolling
  :defer t
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 5)
  (setq scroll-conservatively 101))

(use-package! treemacs
  :defer t
  :config
  (setq treemacs-width 40))

(use-package! eglot
  :defer t
  :config
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider
                                            :documentOnTypeFormattingProvider
                                            :documentRangeFormattingProvider
                                            :documentFormattingProvider
                                            :codeActionProvider))
  (setq eglot-extend-to-xref t)
  (add-to-list 'eglot-server-programs
               '(c-mode . ("ccls")))
  (add-to-list 'eglot-server-programs
               '(c++-mode . ("ccls")))
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio"))))

(use-package! company
  :defer t
  :config
  (setq company-idle-delay 0.5)
  (setq company-minimum-prefix-length 1)
  (setq company-inhibit-inside-symbols t))

(use-package! gt
  :defer t
  :config
  (setq gt-default-translator (gt-translator :engines (gt-youdao-dict-engine)))
  (setq gt-langs '(en zh))
  (setq gt-taker-text 'word)
  (setq gt-taker-pick 'paragraph)
  (setq gt-taker-prompt nil))

(with-eval-after-load 'org
  (setq org-preview-latex-process-alist
        '((dvisvgm
           :programs ("latex" "dvisvgm")
           :description "dvi > svg"
           :message "you need to install the programs: latex and dvisvgm."
           :image-input-type "dvi"
           :image-output-type "svg"
           :image-size-adjust (1.0 . 1.0)
           :latex-compiler ("latex -interaction=nonstopmode -output-directory=%o %f")
           :image-converter ("dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts %f -o %O"))))
  ;; Ensure proper temp directory
  (setq org-preview-latex-image-directory "~/.config/emacs/ltximg/")
  (setq org-latex-create-formula-image-program 'dvisvgm)
  (setq org-support-shift-select t)
  (setq org-latex-pdf-process '("xelatex -shell-escape %f"
                                "xelatex -shell-escape %f"
                                "xelatex -shell-escape %f")))

(use-package! latex
  :defer t
  :config
  (setq latex-run-command "xelatex"))

(use-package! gptel
  :defer t
  :config
  (setq gptel-model 'gpt-4o
        gptel-backend (gptel-make-gh-copilot "Copilot"))
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(use-package! copilot
  :defer t
  ;; :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))

  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(clojure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))

(use-package! rime
  :defer t
  :custom
  (default-input-method "rime"))

(use-package! avy
  :defer t
  :bind
  ("M-g c" . avy-goto-char-timer)
  ("M-g g" . avy-goto-line)
  ("M-g r" . avy-copy-region)
  ("M-g l" . avy-copy-line)
  ("M-g w" . avy-goto-word-1))

(use-package! counsel
  :defer t
  :config
  (counsel-mode 1)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-c f z") 'counsel-fzf))

(after! (prog-mode demap)
  (face-spec-set 'demap-minimap-font-face
                 `((t :family "minimap"
                      :height 20)))
  (setq demap-minimap-window-width 25))


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Iosevka" :weight 'medium :size 14.0))
(defun my-cjk-font()
  (dolist (charset '(kana han cjk-misc symbol bopomofo))
    (set-fontset-font t charset (font-spec :family "Sarasa Mono SC"))))
(add-hook 'after-setting-font-hook #'my-cjk-font)
;;
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-one)
;;(setq doom-theme 'doom-palenight)
;;(setq doom-theme 'doom-nord)
;;(setq doom-theme 'doom-ayu-mirage)
;;(setq doom-theme 'doom-ayu-dark)
;;(setq doom-theme 'doom-city-lights)
(setq doom-theme 'doom-moonlight)
;;(setq doom-theme 'doom-gruvbox)
;;(setq doom-theme 'doom-monokai-pro)
;;(setq doom-theme 'doom-dracula)
;;(setq doom-theme 'gruber-darker)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/")


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
