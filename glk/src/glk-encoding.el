;;; emacs glx  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2008, 2009 Andrew Cowper
;;
;; Author: Andrew Cowper (andrew.cowper@slothrop.net)
;;
;; URL: http://github.com/bloat/emacs-glulx
;;
;; This file is licensed under the terms of the GNU General Public
;; License as distributed with Emacs (press C-h C-c to view it).

(defun glk-char-to-lower (ch)
  (downcase ch))

(defun glk-char-to-upper (ch)
  (upcase ch))

(defun glk-buffer-to-lower-case-uni (string)
  (let ((result (downcase string)))
    (list (length result) result)))

(defun glk-buffer-to-upper-case-uni (string)
  (let ((result (upcase string)))
    (list (length result) result)))

(defun glk-buffer-to-title-case-uni (string lowerrest)
  (let ((result
         (concat (list (upcase (aref string 0)))
                 (if lowerrest
                     (downcase (substring string 1))
                   (substring string 1)))))
    (list (length result) result)))

(provide 'glk-encoding)
