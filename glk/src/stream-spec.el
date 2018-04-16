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

(cl-macrolet ((clean-up-and-check (&body body)
                                  `(unwind-protect
                                       (progn ,@body)
                                     (glki-kill-all-windows)
                                     (glki-kill-all-streams)
                                     (glki-kill-all-filerefs)
                                     (should-not glki-opq-window)
                                     (should-not glki-opq-stream)
                                     (should-not glki-opq-fileref)
                                     (should-not (cl-remove-if #'(lambda (b) (not (string-match "\\*glk\\*" (buffer-name b)))) (buffer-list))))))
  
  (ert-deftest glki-create-window-stream-should-create-a-window-stream ()
    "glki-create-window-stream should create a window stream"
    :tags '(glk stream)
    (clean-up-and-check
     (let* ((buffer (get-buffer-create "*glk*"))
            (stream (glki-create-window-stream 'glk-wintype-text-buffer buffer 'a-stream)))
       (should (eq (glki-opq-stream-buffer stream) buffer))
       (should (eq (glki-opq-stream-type stream) 'glki-window-stream-text-buffer)))))

  (ert-deftest glki-create-window-stream-should-add-the-stream-to-the-collection ()
    "glki-create-window-stream should add the stream to the collection"
    :tags '(glk stream)
    (clean-up-and-check
     (let* ((buffer (get-buffer-create "*glk*"))
            (stream (glki-create-window-stream 'glk-wintype-text-buffer buffer 'a-stream)))
       (should (equal (glk-stream-iterate nil) (list stream 0))))))

  (ert-deftest glk-set-window-should-set-the-current-stream-to-be-that-buffer ()
    "glk-set-window should set the current stream to be that buffer"
    :tags '(glk stream)
    (clean-up-and-check
     (let ((window (glki-generate-new-window 'glk-wintype-text-buffer 'window 'a-stream 0)))
       (glk-set-window window)
       (should (eq glk-current-stream (glki-opq-window-stream window))))))

  (ert-deftest glk-put-string-should-write-to-current-stream-and-propertize-the-text ()
    "glk-put-string should write to current stream and propertize the text"
    :tags '(glk stream)
    (clean-up-and-check
     (let ((window (glki-generate-new-window 'glk-wintype-text-buffer 'window 'a-stream 0)))
       (glk-set-window window)
       (glk-put-string "You are in a room")
       (with-current-buffer (glki-opq-window-buffer window)
         (should (equal (buffer-string) "You are in a room"))
         (should (get-text-property 0 'glk-text (buffer-string)))
         (should (get-text-property 1 'glk-text (buffer-string)))))))

  (ert-deftest glk-put-char-should-write-to-current-stream ()
    "glk-put-char should write to current stream"
    :tags '(glk stream)
    (clean-up-and-check
     (let ((window (glki-generate-new-window 'glk-wintype-text-buffer 'window 'a-stream 0)))
       (glk-set-window window)
       (glk-put-char ?Y)
       (with-current-buffer (glki-opq-window-buffer window)
         (should (equal (buffer-string) "Y"))))))

  (ert-deftest glk-put-char-stream-should-write-to-the-given-stream ()
    "glk-put-char-stream should write to the given stream"
    :tags '(glk stream)
    (clean-up-and-check
     (let ((stream (glki-opq-window-stream (glki-generate-new-window 'glk-wintype-text-buffer 'window 'a-stream 0))))
       (glk-put-char-stream stream ?Y)
       (with-current-buffer (glki-opq-stream-buffer stream)
         (should (equal (buffer-string) "Y"))))))

  (ert-deftest glk-put-string-stream-should-write-to-the-given-stream ()
    "glk-put-string-stream should write to the given stream"
    :tags '(glk stream)
    (clean-up-and-check
     (let ((stream (glki-create-window-stream 'glk-wintype-text-buffer (generate-new-buffer "*glk*") 'a-stream)))
       (glk-put-string-stream stream "You are in a room")
       (with-current-buffer (glki-opq-stream-buffer stream)
         (should (equal (buffer-string) "You are in a room"))))))

  (ert-deftest glk-get-char-stream-should-read-from-the-given-stream ()
    "glk-get-char-stream should read from the given stream"
    :tags '(glk stream)
    (clean-up-and-check
     (let ((stream (glki-opq-window-stream (glki-generate-new-window 'glk-wintype-text-buffer 'window 'a-stream 0))))
       (with-current-buffer (glki-opq-stream-buffer stream)
         (insert "You are in a room")
         (goto-char (point-min)))
       (should (eq (glk-get-char-stream stream) ?Y))
       (should (eq (glk-get-char-stream stream) ?o)))))

  (ert-deftest glk-get-buffer-stream-should-read-from-the-given-stream ()
    "glk-get-buffer-stream should read from the given stream"
    :tags '(glk stream)
    (clean-up-and-check
     (let ((stream (glki-opq-window-stream (glki-generate-new-window 'glk-wintype-text-buffer 'window 'a-stream 0))))
       (with-current-buffer (glki-opq-stream-buffer stream)
         (insert "You are in a room")
         (goto-char (point-min)))
       (should (equal (glk-get-buffer-stream stream 5) '(5 "You a")))
       (should (equal (glk-get-buffer-stream stream 6) '(6 "re in ")))
       (should (equal (glk-get-buffer-stream stream 20) '(6 "a room"))))))

  (ert-deftest glk-put-string-should-fill-strings ()
    "glk-put-string should fill strings"
    :tags '(glk stream)
    (clean-up-and-check
     (let ((window (glki-generate-new-window 'glk-wintype-text-buffer 'window 'a-stream 0)))
       (glk-set-window window)
       (with-current-buffer (glki-opq-window-buffer window)
         (setq fill-column 4)
         (glk-put-string "You are in a room")
         (should (equal (buffer-string) "You\nare\nin a\nroom"))))))

  (ert-deftest glk-stream-open-memory-should-create-an-empty-buffer ()
    "glk-stream-open-memory should create an empty buffer"
    :tags '(glk stream)
    (clean-up-and-check
     (let ((stream (glk-stream-open-memory 'buffer 10 'write 'rock 'a-stream)))
       (should (equal (glki-opq-stream-buffer stream) (get-buffer "*glk*")))
       (should (equal (glki-opq-stream-type stream) 'glki-memory-stream)))))

  (ert-deftest glk-put-string-should-not-fill-or-propertize-a-memory-stream ()
    "glk-put-string should not fill or propertize a memory stream"
    :tags '(glk stream)
    (clean-up-and-check
     (let ((stream (glk-stream-open-memory 'buffer 20 'write 'rock 'a-stream)))
       (with-current-buffer (glki-opq-stream-buffer stream)
         (setq fill-column 4)
         (glk-put-string-stream stream "You are in a room")
         (should (equal (buffer-string) "You are in a room"))
         (should-not (get-text-property 0 'glk-text (buffer-string)))))))

  (ert-deftest a-window-stream-can-not-be-closed ()
    "a window stream can not be closed"
    :tags '(glk stream)
    (clean-up-and-check
     (let ((stream (glki-create-window-stream 'glk-wintype-text-buffer  (get-buffer-create "*glk*") 'a-stream)))
       (should-error (glk-stream-close stream) :type 'glk-error))))

  (ert-deftest closing-a-stream-kills-its-emacs-buffer ()
    "closing a stream kills its emacs buffer"
    :tags '(glk stream)
    (clean-up-and-check
     (let ((stream (glk-stream-open-memory 'buffer 20 'write 'rock 'a-stream)))
       (glk-stream-close stream))
     (should-not (get-buffer "*glk*"))))

  (ert-deftest an-untouched-stream-returns-0-counts-and-its-contents ()
    "an untouched stream returns 0 counts and its contents"
    :tags '(glk stream)
    (clean-up-and-check
     (let ((stream (glk-stream-open-memory 'buffer 20 'write 'rock 'a-stream)))
       (should (equal (glk-stream-close stream) '(nil (t 0 0 nil buffer "")))))))

  (ert-deftest a-memory-stream-returns-its-correct-write-count-and-its-contents ()
    "a stream returns its correct write-count and its contents"
    :tags '(glk stream)
    (clean-up-and-check
     (let ((a-stream (glk-stream-open-memory 'buffer 20 'write 'rock 'a-stream)))
       (glk-put-string-stream a-stream "hello")
       (should (equal (glk-stream-close a-stream) '(nil (t 0 5 nil buffer "hello")))))))

  (ert-deftest setting-a-face-on-a-stream ()
    "Setting a face on a stream"
    :tags '(glk stream)
    (clean-up-and-check
     (let* ((buffer (get-buffer-create "*glk*"))
            (stream (glki-create-window-stream 'glk-wintype-text-buffer buffer 'a-stream)))
       (glk-set-style-stream stream 'glk-emphasized-face)
       (glk-put-string-stream stream "You are in a room")
       (should (equal (get-text-property 5 'face (get-buffer "*glk*")) 'glk-emphasized-face)))))

  (ert-deftest setting-a-face-on-the-current-stream ()
    "Setting a face on the current stream"
    :tags '(glk stream)
    (clean-up-and-check
     (let* ((buffer (get-buffer-create "*glk*"))
            (stream (glki-create-window-stream 'glk-wintype-text-buffer buffer 'a-stream)))
       (glk-stream-set-current stream)
       (glk-set-style 'glk-header-face)
       (glk-put-string-stream stream "You are in a room")
       (should (equal (get-text-property 5 'face (get-buffer "*glk*")) 'glk-header-face)))))

  (ert-deftest glk-stream-open-file-read-mode-should-check-if-file-exists ()
    "Open read mode file checks for existence"
    :tags '(glk stream)
    (clean-up-and-check
     (let* ((non-existing-file-name (make-temp-name "glkert"))
            (non-existing (glk-fileref-create-by-name 'glk-filemode-read non-existing-file-name 0 'non-existing)))
       (should-error (glk-stream-open-file non-existing 'glk-filemode-read 0 'a-stream) :type 'glk-error)
       (should-error (glk-stream-open-file-uni non-existing 'glk-filemode-read 0 'a-stream) :type 'glk-error))))

  (ert-deftest glk-stream-open-file-read-mode-should-load-file-into-buffer ()
    "Open read mode loads file"
    :tags '(glk stream)
    (clean-up-and-check
     (let ((file (make-temp-name "glkert")))
       (unwind-protect
           (let ((existing (glk-fileref-create-by-name 'glk-filemode-read file 0 'existing)))
             (with-temp-file file (insert "glk test file"))
             (let ((stream (glk-stream-open-file existing 'glk-filemode-read 0 'a-stream)))
               (with-current-buffer (glki-opq-stream-buffer stream)
                 (should (string= (buffer-string) "glk test file")))))
         (delete-file file)))))

  (ert-deftest glk-stream-open-file-write-mode-should-create-an-empty-buffer ()
    "glk-stream-open-file write mode should create an empty buffer"
    :tags '(glk stream)
    (clean-up-and-check
     (let ((stream (glk-stream-open-file (glk-fileref-create-by-name 'glk-filemode-write "filename" 0 'existing) 'glk-filemode-write 0 'a-stream)))
       (should (equal (glki-opq-stream-buffer stream) (get-buffer "*glk*")))
       (should (equal (glki-opq-stream-type stream) 'glki-file-stream)))))

  (ert-deftest a-file-stream-returns-nothing ()
    "a file stream returns nothing"
    :tags '(glk stream)
    (clean-up-and-check
     (let* ((existing (glk-fileref-create-by-name 'glk-filemode-write (make-temp-name "glk") 0 'existing))
            (a-stream (glk-stream-open-file existing 'glk-filemode-write 0 'a-stream)))
       (should (equal (glk-stream-close a-stream) (list nil (list nil))))))))
