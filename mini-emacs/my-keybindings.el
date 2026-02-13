;;; my-keybindings.el --- My keybindings -*- lexical-binding:t -*-

(define-key key-translation-map (kbd "C-z") (kbd "C-g"))

;; global keymap
(global-set-key (kbd "C-<backspace>") 'yhu/backward-kill-word)
(global-set-key (kbd "M-d") 'yhu/kill-word)
(global-set-key (kbd "C-M-=") 'eval-print-last-sexp)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-<up>") 'yhu/move-line-up)
(global-set-key (kbd "M-<down>") 'yhu/move-line-down)
(global-set-key (kbd "C-c w") 'wdired-change-to-wdired-mode)
(global-set-key (kbd "M-p") 'yhu/dup-line-and-move-to-last-new-line)
(global-set-key (kbd "M-P") 'yhu/dup-line-and-move-to-original-line)
(global-set-key (kbd "C-M-P") 'copy-from-above-command)
(global-set-key (kbd "C-;") 'yhu/mark-line)
(global-set-key (kbd "C-`") 'compile)
(global-set-key (kbd "C-.") 'mark-word)
(global-set-key (kbd "C-,") 'mark-sexp)
(global-set-key (kbd "C-n") 'yhu/scroll-up-line)
(global-set-key (kbd "C-p") 'yhu/scroll-down-line)

;;; my-keybindings.el ends here
