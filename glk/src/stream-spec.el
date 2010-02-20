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

(context "Streams: "
         (tag streams)

         (macrolet ((clean-up-and-check (&body body)
                                        `(unwind-protect
                                             (progn ,@body)
                                           (glki-dispose-window 'window)
                                           (glki-stream-dispose 'a-stream)
                                           (expect glki-opq-window equals nil "Windows not cleaned up")
                                           (expect glki-opq-stream equals nil "Streams not cleaned up")
                                           (expect (remove-if #'(lambda (b) (not (string-match "\\*glk\\*" (buffer-name b)))) (buffer-list)) equals nil "Buffers not cleaned up"))))

           (specify "glki-create-window-stream should create a window stream"
                    (clean-up-and-check
                     (let ((buffer (get-buffer-create "*glk*")))
                       (put 'window 'buffer buffer)
                       (let ((stream (glki-create-window-stream 'window 'a-stream)))
                         (expect (glki-opq-stream-get-buffer stream) equals buffer)
                         (expect (glki-opq-stream-get-type stream) equals 'glki-window-stream)))))

           (specify "glki-create-window-stream should add the stream to the collection"
                    (clean-up-and-check
                     (let ((buffer (get-buffer-create "*glk*")))
                       (put 'window 'buffer buffer)
                       (let ((stream (glki-create-window-stream 'window 'a-stream)))
                         (expect (glk-stream-iterate nil) equals (list stream nil))))))

           (specify "glk-set-window should set the current stream to be that buffer"
                    (clean-up-and-check
                     (put 'window 'stream 'a-stream)
                     (glk-set-window 'window)
                     (expect glk-current-stream equals 'a-stream)))

           (specify "glk-put-string should write to current stream and propertize the text"
                    (clean-up-and-check
                     (glki-generate-new-window 'glk-wintype-text-buffer 'window 'a-stream)
                     (glk-set-window 'window)
                     (glk-put-string "You are in a room")
                     (save-current-buffer
                       (set-buffer "*glk*")
                       (expect (buffer-string) equals "You are in a room")
                       (expect (get-text-property 0 'glk-text (buffer-string)))
                       (expect (get-text-property 1 'glk-text (buffer-string))))))

           (specify "glk-put-char should write to current stream"
                    (clean-up-and-check
                     (glki-generate-new-window 'glk-wintype-text-buffer 'window 'a-stream)
                     (glk-set-window 'window)
                     (glk-put-char ?Y)
                     (save-current-buffer
                       (set-buffer "*glk*")
                       (expect (buffer-string) equals "Y"))))

           (specify "glk-put-char-stream should write to the given stream"
                    (clean-up-and-check
                     (put 'a-stream 'buffer (generate-new-buffer "*glk*"))
                     (glk-put-char-stream 'a-stream ?Y)
                     (save-current-buffer
                       (set-buffer "*glk*")
                       (expect (buffer-string) equals "Y"))))

           (specify "glk-put-string-stream should write to the given stream"
                    (clean-up-and-check
                     (put 'a-stream 'buffer (generate-new-buffer "*glk*"))
                     (glk-put-string-stream 'a-stream "You are in a room")
                     (save-current-buffer
                       (set-buffer "*glk*")
                       (expect (buffer-string) equals "You are in a room"))))

           (specify "glk-get-char-stream should read from the given stream"
                    (clean-up-and-check
                     (put 'a-stream 'buffer (generate-new-buffer "*glk*"))
                     (save-current-buffer
                       (set-buffer "*glk*")
                       (insert "You are in a room")
                       (goto-char (point-min)))
                     (expect (glk-get-char-stream 'a-stream) equals ?Y)
                     (expect (glk-get-char-stream 'a-stream) equals ?o)))

           (specify "glk-get-buffer-stream should read from the given stream"
                    (clean-up-and-check
                     (put 'a-stream 'buffer (generate-new-buffer "*glk*"))
                     (save-current-buffer
                       (set-buffer "*glk*")
                       (insert "You are in a room")
                       (goto-char (point-min)))
                     (expect (glk-get-buffer-stream 'a-stream 5) equals '("You a" 5))
                     (expect (glk-get-buffer-stream 'a-stream 6) equals '("re in " 6))))

           (specify "glk-put-string should fill strings"
                    (clean-up-and-check
                     (glki-generate-new-window 'glk-wintype-text-buffer 'window 'a-stream)
                     (glk-set-window 'window)
                     (save-excursion
                       (set-buffer "*glk*")
                       (setq fill-column 4)
                       (glk-put-string "You are in a room")
                       (expect (buffer-string) equals "You\nare\nin a\nroom"))))

           (specify "glk-stream-open-memory should create an empty buffer"
                    (clean-up-and-check
                     (glk-stream-open-memory 'buffer 10 'write 'rock 'a-stream)
                     (expect (glki-opq-stream-get-buffer 'a-stream) equals (get-buffer "*glk*"))
                     (expect (glki-opq-stream-get-type 'a-stream) equals 'glki-memory-stream)))

           (specify "glk-put-string should not fill or propertize a memory stream"
                    (clean-up-and-check
                     (glk-stream-open-memory 'buffer 20 'write 'rock 'a-stream)
                     (save-excursion
                       (set-buffer "*glk*")
                       (setq fill-column 4)
                       (glk-put-string-stream 'a-stream "You are in a room")
                       (expect (buffer-string) equals "You are in a room")
                       (expect (not (get-text-property 0 'glk-text (buffer-string)))))))

           (specify "a window stream can not be closed"
                    (clean-up-and-check
                     (let ((buffer (get-buffer-create "*glk*")))
                       (put 'window 'buffer buffer)
                       (let ((stream (glki-create-window-stream 'window 'a-stream)))
                         (expect (glk-stream-close 'a-stream) throws glk-error)))))

           (specify "closing a stream kills its emacs buffer"
                    (clean-up-and-check
                     (glk-stream-open-memory 'buffer 20 'write 'rock 'a-stream)
                     (glk-stream-close 'a-stream)
                     (expect (not (get-buffer "*glk*")))))

           (specify "an untouched stream returns 0 counts and its contents"
                    (clean-up-and-check
                     (glk-stream-open-memory 'buffer 20 'write 'rock 'a-stream)
                     (expect (glk-stream-close 'a-stream) equals '(nil (0 0 buffer "")))))

           (specify "an stream returns its correct write-count and its contents"
                    (clean-up-and-check
                     (glk-stream-open-memory 'buffer 20 'write 'rock 'a-stream)
                     (glk-put-string-stream 'a-stream "hello")
                     (expect (glk-stream-close 'a-stream) equals '(nil (0 5 buffer "hello")))))

           (specify "Setting a face on a stream"
                    (clean-up-and-check
                     (put 'a-stream 'buffer (generate-new-buffer "*glk*"))
                     (glk-set-style-stream 'a-stream 'glk-emphasized-face)
                     (glk-put-string-stream 'a-stream "You are in a room")
                     (expect (get-text-property 5 'face (get-buffer "*glk*")) equals 'glk-emphasized-face)))

           (specify "Setting a face on the current stream"
                    (clean-up-and-check
                     (put 'a-stream 'buffer (generate-new-buffer "*glk*"))
                     (glk-stream-set-current 'a-stream)
                     (glk-set-style 'glk-header-face)
                     (glk-put-string-stream 'a-stream "You are in a room")
                     (expect (get-text-property 5 'face (get-buffer "*glk*")) equals 'glk-header-face)))))
