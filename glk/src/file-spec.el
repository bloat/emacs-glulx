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



