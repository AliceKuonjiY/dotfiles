;;; my-code.el --- My code -*- lexical-binding:t -*-

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
        (while (looking-back "[a-zA-Z0-9]")
          (backward-delete-char 1))
      (backward-delete-char 1))))

(defun yhu/kill-word ()
  "Remove all whitespace if the character behind the cursor is whitespace, otherwise remove a word."
  (interactive)
  (if (looking-at "[^a-zA-Z0-9\n]")
      ;; delete horizontal space before us and then check to see if we
      ;; are looking at a newline
      (progn (delete-horizontal-space 't)
             (while (looking-at "[^a-zA-Z0-9\n]")
               (delete-char 1)))
    ;; otherwise, just do the normal kill word.
    (if (looking-at "[a-zA-Z0-9]")
        (while (looking-at "[a-zA-Z0-9]")
          (delete-char 1))
      (delete-char 1))))


(defun yhu/move-line-up ()
  "Move the current line up by one line."
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (forward-char col)))

(defun yhu/move-line-down ()
  "Move the current line down by one line."
  (interactive)
  (let ((col (current-column)))
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (forward-char col)))

(defun yhu/mark-line ()
  "Mark the current line."
  (interactive)
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (move-end-of-line nil))

;;; my-code.el ends here
