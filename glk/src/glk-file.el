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

(require 'glk-opaque)

(defopaque fileref filename)

(defun glk-fileref-create-by-name (usage name rock new-fileref)
  (glki-opq-fileref-create new-fileref)
  (glki-opq-fileref-set-filename new-fileref name)
  (glki-opq-fileref-set-rock new-fileref rock)
  new-fileref)

(defun glk-fileref-create-by-prompt (usage fmode rock new-fileref)
  (glk-fileref-create-by-name usage (read-from-minibuffer "file name") rock new-fileref))

(defun glk-fileref-create-temp (usage rock new-fileref)
  (glk-fileref-create-by-name usage (make-temp-name "glk") rock new-fileref))

(defun glk-fileref-destroy (fref)
  (glki-opq-fileref-dispose fref))

(defun glk-fileref-does-file-exist (fileref)
  (file-exists-p (glki-get-filename fileref)))

(defun glki-get-filename (fileref)
  "Get the filename referred to by this fileref"
  (glki-opq-fileref-get-filename fileref))

(defun glki-kill-all-filerefs ()
  "Cleans up all glk streams and destroys the associated buffers"
  (mapcar #'glki-opq-fileref-dispose glki-opq-fileref))

(provide 'glk-file)
