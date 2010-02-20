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

(require 'glk-function-dispatch)
(require 'glk-window)
(require 'glk-event)

(put 'glk-error 'error-conditions '(error glk-error))
(put 'glk-error 'error-message "Glk error")

(defun glk-start ()
  "This function is used to initialised the GLK library and start the subprocess which will be using it."
  (interactive)
  (glki-init (make-frame))
  (setq glk-process (start-process "glk-proc" "glk-command-stream" "~/dev/src/lisp/elisp/glk/glk-prog/hello_elisp_glk"))
  (set-process-filter glk-process #'glki-call-function)
  (generate-new-buffer "*glk-log*"))

(defun glki-end ()
  "This function is used to clean up after GLK library and its subprocess have finished"
  (interactive)
  (when (and glk-process (eq 'run (process-status glk-process))) (quit-process glk-process))
  (when glk-process (kill-buffer (process-buffer glk-process)))
  (setq glk-process nil)
  (when glk-frame (delete-frame glk-frame))
  (setq glk-frame nil)
  (glki-kill-all-streams)
  (glki-kill-all-windows)
  (setq glk-root-window nil)
  (setq glk-event-queue nil)
  (setq glk-select-waiting nil)
  (setq glk-current-stream nil)
  (when (get-buffer "*glk-log*") (kill-buffer "*glk-log*")))

(defun glki-set-game-file-stream (p))

(defun glk-exit ()
  (glki-end))

(defun glk-gestalt (sel var)
  (case sel
    ('glk-gestalt-version #x700)))

(provide 'glk)
