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

(ert-deftest glk-fileref-create-by-name-should-create-a-fileref ()
  "glk-fileref-create-by-name should create a fileref"
  :tags '(glk file)
  (unwind-protect
      (progn
        (glk-fileref-create-by-name 0 "myfile" 0 'fileref)
        (should (equal (glki-get-filename 'fileref) "myfile")))
    (glki-opq-fileref-dispose 'fileref)))

(ert-deftest glk-fileref-create-by-prompt-should-create-a-fileref ()
  "glk-fileref-create-by-prompt should create a fileref"
  :tags '(glk file)
  (cl-flet ((test-read-from-minibuffer (prompt) "mockedfilename"))
    (unwind-protect
        (progn
          (advice-add 'read-from-minibuffer :override #'test-read-from-minibuffer '((name . test-read-from-minibuffer)))
          (glk-fileref-create-by-prompt 0 'glk-filemode-read 0 'fileref)
          (should (equal (glki-get-filename 'fileref) "mockedfilename")))
      (advice-remove 'read-from-minibuffer 'test-read-from-minibuffer)
      (glki-opq-fileref-dispose 'fileref))))

(ert-deftest glk-fileref-destroy-should-remove-the-fileref ()
  "glk-fileref-destroy should remove the fileref"
  :tags '(glk file)
  (glk-fileref-create-by-name 0 "myfile" 0 'fileref)
  (glk-fileref-destroy 'fileref)
  (should-not (glki-get-filename 'fileref)))



