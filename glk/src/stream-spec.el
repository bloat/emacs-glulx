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

(macrolet ((clean-up-and-check (&body body)
                               `(unwind-protect
                                    (progn ,@body)
                                  (glki-dispose-window 'window)
                                  (glki-stream-dispose 'a-stream)
                                  (should-not glki-opq-window)
                                  (should-not glki-opq-stream)
                                  (should-not (remove-if #'(lambda (b) (not (string-match "\\*glk\\*" (buffer-name b)))) (buffer-list))))))

  (ert-deftest glki-create-window-stream-should-create-a-window-stream ()
    "glki-create-window-stream should create a window stream"
    :tags '(glk stream)
    (clean-up-and-check
     (let ((buffer (get-buffer-create "*glk*")))
       (put 'window 'buffer buffer)
       (let ((stream (glki-create-window-stream 'window 'a-stream)))
         (should (equal (glki-opq-stream-get-buffer stream) buffer))
         (should (equal (glki-opq-stream-get-type stream) 'glki-window-stream))))))

  (ert-deftest glki-create-window-stream-should-add-the-stream-to-the-collection ()
    "glki-create-window-stream should add the stream to the collection"
    :tags '(glk stream)
    (clean-up-and-check
     (let ((buffer (get-buffer-create "*glk*")))
       (put 'window 'buffer buffer)
       (let ((stream (glki-create-window-stream 'window 'a-stream)))
         (should (equal (glk-stream-iterate nil) (list stream nil)))))))

  (ert-deftest glk-set-window-should-set-the-current-stream-to-be-that-buffer ()
    "glk-set-window should set the current stream to be that buffer"
    :tags '(glk stream)
    (clean-up-and-check
     (put 'window 'stream 'a-stream)
     (glk-set-window 'window)
     (should (equal glk-current-stream 'a-stream))))

  (ert-deftest glk-put-string-should-write-to-current-stream-and-propertize-the-text ()
    "glk-put-string should write to current stream and propertize the text"
    :tags '(glk stream)
    (clean-up-and-check
     (glki-generate-new-window 'glk-wintype-text-buffer 'window 'a-stream 0)
     (glk-set-window 'window)
     (glk-put-string "You are in a room")
     (save-current-buffer
       (set-buffer "*glk*")
       (should (equal (buffer-string) "You are in a room"))
       (should (get-text-property 0 'glk-text (buffer-string)))
       (should (get-text-property 1 'glk-text (buffer-string))))))

  (ert-deftest glk-put-char-should-write-to-current-stream ()
    "glk-put-char should write to current stream"
    :tags '(glk stream)
    (clean-up-and-check
     (glki-generate-new-window 'glk-wintype-text-buffer 'window 'a-stream 0)
     (glk-set-window 'window)
     (glk-put-char ?Y)
     (save-current-buffer
       (set-buffer "*glk*")
       (should (equal (buffer-string) "Y")))))

  (ert-deftest glk-put-char-stream-should-write-to-the-given-stream ()
    "glk-put-char-stream should write to the given stream"
    :tags '(glk stream)
    (clean-up-and-check
     (put 'a-stream 'buffer (generate-new-buffer "*glk*"))
     (glk-put-char-stream 'a-stream ?Y)
     (save-current-buffer
       (set-buffer "*glk*")
       (should (equal (buffer-string) "Y")))))

  (ert-deftest glk-put-string-stream-should-write-to-the-given-stream ()
    "glk-put-string-stream should write to the given stream"
    :tags '(glk stream)
    (clean-up-and-check
     (put 'a-stream 'buffer (generate-new-buffer "*glk*"))
     (glk-put-string-stream 'a-stream "You are in a room")
     (save-current-buffer
       (set-buffer "*glk*")
       (should (equal (buffer-string) "You are in a room")))))

  (ert-deftest glk-get-char-stream-should-read-from-the-given-stream ()
    "glk-get-char-stream should read from the given stream"
    :tags '(glk stream)
    (clean-up-and-check
     (put 'a-stream 'buffer (generate-new-buffer "*glk*"))
     (save-current-buffer
       (set-buffer "*glk*")
       (insert "You are in a room")
       (goto-char (point-min)))
     (should (equal (glk-get-char-stream 'a-stream) ?Y))
     (should (equal (glk-get-char-stream 'a-stream) ?o))))

  (ert-deftest glk-get-buffer-stream-should-read-from-the-given-stream ()
    "glk-get-buffer-stream should read from the given stream"
    :tags '(glk stream)
    (clean-up-and-check
     (put 'a-stream 'buffer (generate-new-buffer "*glk*"))
     (save-current-buffer
       (set-buffer "*glk*")
       (insert "You are in a room")
       (goto-char (point-min)))
     (should (equal (glk-get-buffer-stream 'a-stream 5) '("You a" 5)))
     (should (equal (glk-get-buffer-stream 'a-stream 6) '("re in " 6)))))

  (ert-deftest glk-put-string-should-fill-strings ()
    "glk-put-string should fill strings"
    :tags '(glk stream)
    (clean-up-and-check
     (glki-generate-new-window 'glk-wintype-text-buffer 'window 'a-stream 0)
     (glk-set-window 'window)
     (save-excursion
       (set-buffer "*glk*")
       (setq fill-column 4)
       (glk-put-string "You are in a room")
       (should (equal (buffer-string) "You\nare\nin a\nroom")))))

  (ert-deftest glk-stream-open-memory-should-create-an-empty-buffer ()
    "glk-stream-open-memory should create an empty buffer"
    :tags '(glk stream)
    (clean-up-and-check
     (glk-stream-open-memory 'buffer 10 'write 'rock 'a-stream)
     (should (equal (glki-opq-stream-get-buffer 'a-stream) (get-buffer "*glk*")))
     (should (equal (glki-opq-stream-get-type 'a-stream) 'glki-memory-stream))))

  (ert-deftest glk-put-string-should-not-fill-or-propertize-a-memory-stream ()
    "glk-put-string should not fill or propertize a memory stream"
    :tags '(glk stream)
    (clean-up-and-check
     (glk-stream-open-memory 'buffer 20 'write 'rock 'a-stream)
     (save-excursion
       (set-buffer "*glk*")
       (setq fill-column 4)
       (glk-put-string-stream 'a-stream "You are in a room")
       (should (equal (buffer-string) "You are in a room"))
       (should-not (get-text-property 0 'glk-text (buffer-string))))))

  (ert-deftest a-window-stream-can-not-be-closed ()
    "a window stream can not be closed"
    :tags '(glk stream)
    (clean-up-and-check
     (let ((buffer (get-buffer-create "*glk*")))
       (put 'window 'buffer buffer)
       (let ((stream (glki-create-window-stream 'window 'a-stream)))
         (should-error (glk-stream-close 'a-stream) :type 'glk-error)))))

  (ert-deftest closing-a-stream-kills-its-emacs-buffer ()
    "closing a stream kills its emacs buffer"
    :tags '(glk stream)
    (clean-up-and-check
     (glk-stream-open-memory 'buffer 20 'write 'rock 'a-stream)
     (glk-stream-close 'a-stream)
     (should-not (get-buffer "*glk*"))))

  (ert-deftest an-untouched-stream-returns-0-counts-and-its-contents ()
    "an untouched stream returns 0 counts and its contents"
    :tags '(glk stream)
    (clean-up-and-check
     (glk-stream-open-memory 'buffer 20 'write 'rock 'a-stream)
     (should (equal (glk-stream-close 'a-stream) '(nil (0 0 nil buffer ""))))))

  (ert-deftest a-stream-returns-its-correct-write-count-and-its-contents ()
    "a stream returns its correct write-count and its contents"
    :tags '(glk stream)
    (clean-up-and-check
     (glk-stream-open-memory 'buffer 20 'write 'rock 'a-stream)
     (glk-put-string-stream 'a-stream "hello")
     (should (equal (glk-stream-close 'a-stream) '(nil (0 5 nil buffer "hello"))))))

  ;; TODO this uses the current stream for the face, when it should use the passed in stream
  (ert-deftest setting-a-face-on-a-stream ()
    "Setting a face on a stream"
    :tags '(glk stream)
    :expected-result :failed
    (clean-up-and-check
     (put 'a-stream 'buffer (generate-new-buffer "*glk*"))
     (glk-set-style-stream 'a-stream 'glk-emphasized-face)
     (glk-put-string-stream 'a-stream "You are in a room")
     (should (equal (get-text-property 5 'face (get-buffer "*glk*")) 'glk-emphasized-face))))

  (ert-deftest setting-a-face-on-the-current-stream ()
    "Setting a face on the current stream"
    :tags '(glk stream)
    (clean-up-and-check
     (put 'a-stream 'buffer (generate-new-buffer "*glk*"))
     (glk-stream-set-current 'a-stream)
     (glk-set-style 'glk-header-face)
     (glk-put-string-stream 'a-stream "You are in a room")
     (should (equal (get-text-property 5 'face (get-buffer "*glk*")) 'glk-header-face)))))
