(defun glki-log (log)
  (when (get-buffer "*glk-log*")
    (save-excursion
      (set-buffer "*glk-log*")
      (insert (if (stringp log) log (prin1-to-string log)))
      (newline))))

(defvar glk-char-input-mode-map
  (let ((map (make-keymap)))
    (define-key map [remap self-insert-command] 'glki-press-any-key)
    (define-key map (kbd "RET") 'glki-press-any-key)
    map))

(provide 'glk-base)
