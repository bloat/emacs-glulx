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


(ert-deftest glk-fileref-create-by-name-should-create-a-fileref ()
  "glk-fileref-create-by-name should create a fileref"
  :tags '(glk file)
  (unwind-protect
      (let ((fileref (glk-fileref-create-by-name 0 "myfile" 0 'fileref)))
        (should (equal (glki-get-filename fileref) "myfile")))
    (glki-kill-all-filerefs)))

(ert-deftest glk-fileref-create-by-prompt-should-create-a-fileref ()
  "glk-fileref-create-by-prompt should create a fileref"
  :tags '(glk file)
  (unwind-protect
      (cl-letf (((symbol-function 'read-from-minibuffer) (lambda (prompt) "mockedfilename")))
        (let ((fileref (glk-fileref-create-by-prompt 0 'glk-filemode-read 0 'fileref)))
          (should (equal (glki-get-filename fileref) "mockedfilename"))))
    (glki-kill-all-filerefs)))

(ert-deftest glk-fileref-create-temp-should-create-a-fileref ()
  "glk-fileref-create-temp should create a fileref"
  :tags '(glk file)
  (unwind-protect
      (let ((fileref (glk-fileref-create-temp 0 0 'fileref)))
        (should (string-prefix-p "glk" (glki-get-filename fileref))))
    (glki-kill-all-filerefs)))

(ert-deftest glk-fileref-destroy-should-remove-the-fileref ()
  "glk-fileref-destroy should remove the fileref"
  :tags '(glk file)
  (unwind-protect
      (let ((fileref (glk-fileref-create-by-name 0 "myfile" 0 'fileref)))
        (glk-fileref-destroy fileref)
        (should-not glki-opq-fileref))
    (glki-kill-all-filerefs)))



