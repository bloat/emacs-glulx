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

(context "Files: "
         (tag files)

         (specify "glk-fileref-create-by-name should create a fileref"
                  (unwind-protect
                      (progn
                        (glk-fileref-create-by-name 0 "myfile" 0 'fileref)
                        (expect (glki-get-filename 'fileref) equals "myfile"))
                    (glki-opq-fileref-dispose 'fileref)))

         (specify "glk-fileref-create-by-prompt should create a fileref"
                  (unwind-protect
                      (flet ((read-from-minibuffer (prompt) "mockedfilename"))
                        (glk-fileref-create-by-prompt 0 'glk-filemode-read 0 'fileref)
                        (expect (glki-get-filename 'fileref) equals "mockedfilename"))
                    (glki-opq-fileref-dispose 'fileref)))

         (specify "glk-fileref-destroy should remove the fileref"
                  (glk-fileref-create-by-name 0 "myfile" 0 'fileref)
                  (glk-fileref-destroy 'fileref)
                  (expect (not (glki-get-filename 'fileref)))))



