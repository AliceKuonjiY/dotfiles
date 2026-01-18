(global-set-key (kbd "C-M-=") 'eval-print-last-sexp)
(global-set-key (kbd "C-<") 'yhu/previous-5-lines)
(global-set-key (kbd "C->") 'yhu/next-5-lines)
(global-set-key (kbd "C-<backspace>") 'yhu/backward-kill-word)
(global-set-key (kbd "M-<up>") 'yhu/move-line-up)
(global-set-key (kbd "M-<down>") 'yhu/move-line-down)
(global-set-key (kbd "C-c w") 'wdired-change-to-wdired-mode)

(defhydra hydra-text-scale (global-map "C-c t s")
  "scale text"
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" (text-scale-set 0) "reset")
  ("q" nil "finished" :exit t))

(defhydra hydra-window-move (global-map "C-x w m")
  "move window"
  ("<left>" windmove-left "move left")
  ("<right>" windmove-right "move right")
  ("<up>" windmove-up "move up")
  ("<down>" windmove-down "move down")
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
