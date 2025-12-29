;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
        ("http" . "127.0.0.1:7897")
        ("https" . "127.0.0.1:7897")))

(add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 1)
(setq company-inhibit-inside-symbols t)
(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq-default c-basic-offset 4)
(setq-default confirm-kill-emacs #'y-or-n-p)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-x C-s") 'isearch-forward)
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "C-x C-f") 'set-fill-column)
(global-set-key (kbd "C-<") 'beginning-of-line)
(global-set-key (kbd "C->") 'end-of-line)

(defhydra hydra-window-size-adjust (global-map "C-c m a")
  "window size adjust"
  ("+" evil-window-increase-height "increase height")
  ("-" evil-window-decrease-height "decrease height")
  (">" evil-window-increase-width "increase width")
  ("<" evil-window-decrease-width "decrease width")
  ("=" balance-windows "balance"))

(defhydra hydra-multiple-cursor (global-map "C-c m g")
  "multiple cursor"
  ("n" evil-mc-make-and-goto-next-match "next match")
  ("p" evil-mc-make-and-goto-prev-match "prev match")
  ("j" evil-mc-make-cursor-move-next-line "next line")
  ("k" evil-mc-make-cursor-move-prev-line "prev line"))

(defhydra hydra-switch-buffer (global-map "C-c m b")
  "switch buffer"
  ("[" previous-buffer "prev buffer")
  ("]" next-buffer "next buffer"))
(push '(undecorated . t) default-frame-alist)
(setq display-time-default-load-average nil)
(setq display-time-24hr-format t)
(display-time-mode t)
;; org config
(with-eval-after-load 'org
  (bind-key "C-c m i +" 'image-increase-size org-mode-map)
  (bind-key "C-c m i -" 'image-decrease-size org-mode-map)
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
  (setq org-latex-create-formula-image-program 'dvisvgm))

(defun read-lines-to-list (file-path)
  "Read the contents of FILE-PATH and return a list of lines."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun create-anime-banner ()
  (let* ((banner (read-lines-to-list "~/.config/doom/ascii-pic/emacs.txt"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'create-anime-banner)

(use-package! gt :ensure t)
(setq gt-langs '(en zh))
(setq gt-default-translator (gt-translator :engines (gt-youdao-dict-engine)))
(setq gt-taker-text 'word)
(setq gt-taker-pick 'paragraph)
(setq gt-taker-prompt nil)

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
;; |“中”、“言”测试|
;; ||||||||||||||||
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-palenight)

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
