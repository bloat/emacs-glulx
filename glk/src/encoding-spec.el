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

(ert-deftest should-be-able-to-convert-to-lower-case ()
  "Should be able to convert to lower case"
  :tags '(glk encoding)
  (should (eql (glk-char-to-lower ?A) ?a))
  (should (eql (glk-char-to-lower ?C) ?c))
  (should (eql (glk-char-to-lower ?b) ?b))
  (should (eql (glk-char-to-lower ?e) ?e)))
