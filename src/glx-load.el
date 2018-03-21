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

(require 'glx-value)
(require 'glx-glulx)
(require 'glx-stack)
(require 'glx-exec)

(require 'glk-event)

(defvar *glx-unexpected-exit* nil "Is the VM exiting unexpectedly, or because it's waiting for player input")

(defun glx-load-story-file (file-name)
  (save-excursion
    (set-buffer (get-buffer-create "*glulx*"))
    (erase-buffer)
    (insert-file-contents-literally file-name)
    (vconcat (mapcar (lambda (x) (logand 255 x)) (buffer-substring (point-min) (point-max))))))

(put 'glx-load-error 'error-conditions '(error glx-error glx-load-error))
(put 'glx-load-error 'error-message "Glulx game file load error")

(defun glx-process-header ()
  (unless (equal (glx-memory-get-32 glx-0) (glx-32 #x6c #x75 #x6c #x47))
    (signal 'glx-load-error (list "Not a Glulx game file" (glx-memory-get-32 glx-0))))
  (unless (or (equal (glx-memory-get-32 glx-4) (glx-32 3 1 3 0))
              (equal (glx-memory-get-32 glx-4) (glx-32 2 1 3 0))
              (equal (glx-memory-get-32 glx-4) (glx-32 1 1 3 0))
              (equal (glx-memory-get-32 glx-4) (glx-32 0 1 3 0))
              (equal (glx-memory-get-32 glx-4) (glx-32 0 0 3 0))
              (equal (glx-memory-get-32 glx-4) (glx-32 0 0 2 0)))
    (signal 'glx-load-error (list "Incorrect Glulx version" (glx-memory-get-32 glx-4))))
  (setq *glx-ram-start* (glx-memory-get-32 glx-8))
  (setq *glx-memory* (expand-memory-vector *glx-memory*))
  (setq *glx-string-table* (glx-memory-get-32 (glx-32 28)))
  (setq *glx-stack* '())
  (glx-memory-get-32 (glx-32 24)))

(defun glx-play-game (file-name)
  (interactive "fGame file name: ")
  (setq *glx-memory* (glx-load-story-file file-name))
  (setq *glx-original-memory* (copy-sequence *glx-memory*))
  (setq *glx-glk-selected* nil)
  (setq *glx-glk-id-gen* 0)
  (setq *glx-pc* nil)
  (setq *glx-unexpected-exit* t)
  (setq *glx-log-buffer* (get-buffer-create "*glx-log*"))
  (setq *glx-undo* nil)
  (setq *glx-protect* nil)
  (setq *glx-iosys* (list (lambda (c)) glx-0 glx-0))
  (setq *glx-compiled-instructions* (make-hash-table :test 'equal :size 100000))
    
  (setq glk-event-reentry-function #'glx-glk-event-callback)
  (save-excursion
    (set-buffer *glx-log-buffer*)
    (erase-buffer))
  (random t)
  (unwind-protect
      (progn
        (glki-init (make-frame))
        (glx-process-header)
        (glx-call-function-and-return-to-emacs (glx-memory-get-32 (glx-32 24)) nil))
    (glx-cleanup)))

(defun glx-cleanup ()
  (when *glx-unexpected-exit*
    (glki-end)
    (setq *glx-stack* nil)
    (setq *glx-string-table* nil)
    (setq *glx-memory* nil)
    (setq *glx-original-memory* nil)
    (setq *glx-ram-start* nil)
    (setq *glx-glk-selected* nil)
    (setq *glx-memory* nil)
    (setq *glx-glk-id-gen* 0)
    (setq *glx-pc* nil)
    (setq *glx-log-buffer* nil)
    (setq *glx-catch-token* glx-0)
    (setq *glx-undo* nil)
    (setq *glx-protect* nil)
    (setq *glx-compiled-instructions* nil)
    (message "glulx finished")))

(provide 'glx-load)

