;;; my-keybindings.el --- My keybindings -*- lexical-binding:t -*-

(define-key key-translation-map (kbd "C-z") (kbd "C-g"))

(global-set-key (kbd "C-M-=") 'eval-print-last-sexp)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-<backspace>") 'yhu/backward-kill-word)
(global-set-key (kbd "M-<up>") 'yhu/move-line-up)
(global-set-key (kbd "M-<down>") 'yhu/move-line-down)
(global-set-key (kbd "C-c w") 'wdired-change-to-wdired-mode)
(global-set-key (kbd "M-p") 'duplicate-line)
(global-set-key (kbd "M-P") 'copy-from-above-command)
(global-set-key (kbd "C-;") 'yhu/mark-line)
(global-set-key (kbd "C-`") 'compile)

(defhydra hydra-text-scale (global-map "C-c t s")
  "scale text"
  ("=" text-scale-increase "increase")
  ("-" text-scale-decrease "decrease")
  ("0" (text-scale-set 0) "reset" :exit t)
  ("q" nil "finished" :exit t))

;;; my-keybindings.el ends here
