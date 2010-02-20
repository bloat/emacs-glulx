;;; emacs glx
;;
;; Copyright (C) 2008, 2009 Andrew Cowper
;;
;; Author: Andrew Cowper (andrew.cowper@slothrop.net)
;;
;; URL: http://github.com/bloat/emacs-glulx
;;
;; This file is licensed under the terms of the GNU General Public
;; License as distributed with Emacs (press C-h C-c to view it).

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
