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

(context "Character encoding: "
         (tag encoding)

         (specify "Should be able to convert to lower case"
                  (expect (glk-char-to-lower ?A) equals ?a)
                  (expect (glk-char-to-lower ?C) equals ?c)
                  (expect (glk-char-to-lower ?b) equals ?b)
                  (expect (glk-char-to-lower ?e) equals ?e)))


