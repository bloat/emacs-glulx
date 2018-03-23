;;; emacs glx
;;
;; Copyright (C) 2008, 2009, 2017, 2018 Andrew Cowper
;;
;; Author: Andrew Cowper (andrew.cowper@slothrop.net)
;;
;; URL: http://github.com/bloat/emacs-glulx
;;
;; This file is licensed under the terms of the GNU General Public
;; License as distributed with Emacs (press C-h C-c to view it).

(require 'glx-glulx)

(defun glx-accelerated-z-region (memptr32)
  (if (glx-neg-p memptr32)
      glx-0
    (let ((memptr (glx-32->int memptr32)))
      (if (and (>= memptr 36) (< memptr (length *glx-memory*)))
          (let ((val (glx-memory-get-byte-int memptr32)))
            (cond ((>= val #xe0) glx-3)
                  ((>= val #xc0) glx-2)
                  ((and (>= val #x70) (<= val #x7f) (>= memptr (glx-32->int *glx-ram-start*))) glx-1)
                  (glx-0)))
        glx-0))))

(provide 'glx-accelerated)
