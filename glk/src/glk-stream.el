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

(require 'glk-opaque)

(defvar glk-current-stream nil
  "The current stream for reading and writing.
Used by routines which do not specify a stream")

(defopaque stream buffer type read-count write-count storage face unicode mode)

(defun glki-process-string-for-insertion (stream s)
  (if (eq (glki-opq-stream-get-type stream) 'glki-memory-stream)
      (progn
        (glki-opq-stream-set-write-count stream
                                         (+ (length s) (glki-opq-stream-get-write-count stream)))
        s)
    (propertize s
                'read-only "Game text is read only"
                'rear-nonsticky t
                'front-sticky '(read-only)
                'glk-text t
                'face (glki-opq-stream-get-face stream)
                'hard t)))

(defun glki-get-current-style-face ()
  (glki-opq-stream-get-face glk-current-stream))

(defun glki-post-process-current-buffer (stream pre-insert-point)
  (unless (eq (glki-opq-stream-get-type stream) 'glki-memory-stream)
    (unless (string= " " (buffer-substring (- (line-end-position) 1) (line-end-position)))
      (fill-region pre-insert-point (line-end-position)))))

(defun glk-put-string-stream (str s)
  (with-current-buffer (glki-opq-stream-get-buffer str)
    (let ((inhibit-read-only t)
          (pre-insert-point (point-max)))
      (insert (glki-process-string-for-insertion str s))
      (glki-post-process-current-buffer str pre-insert-point))
    (goto-char (point-max)))
  nil)

(defun glk-put-string (s)
  (glk-put-string-stream glk-current-stream s))

(defun glk-put-char-stream (str ch)
  (glk-put-string-stream str (char-to-string ch)))

(defun glk-put-char (ch)
  (glk-put-char-stream glk-current-stream ch))

(defun glk-get-char-stream (str)
  (with-current-buffer (glki-opq-stream-get-buffer str)
    (let ((char (char-after)))
      (forward-char)
      char)))

(defun glk-get-buffer-stream (str len)
  (let ((result-string ""))
    (dotimes (var len (list result-string (length result-string)))
      (setq result-string (concat result-string (list (glk-get-char-stream str)))))))

(defun glk-stream-get-current ()
  glk-current-stream)

(defun glk-stream-set-current (str)
  (setq glk-current-stream str))

(defun glki-stream-open-memory (buf buflen fmode rock stream-id unicode)
  (let ((stream (glki-opq-stream-create stream-id)))
    (glki-opq-stream-set-buffer stream (generate-new-buffer "*glk*"))
    (glki-opq-stream-set-type stream 'glki-memory-stream)
    (glki-opq-stream-set-read-count stream 0)
    (glki-opq-stream-set-write-count stream 0)
    (glki-opq-stream-set-storage stream buf)
    (glki-opq-stream-set-unicode stream unicode)
    (glki-opq-stream-set-mode stream fmode)
    (glki-opq-stream-set-rock stream rock)
    stream))

(defun glk-stream-open-memory (buf buflen fmode rock stream-id)
  (glki-stream-open-memory buf buflen fmode rock stream-id nil))

(defun glk-stream-open-memory-uni (buf buflen fmode rock stream-id)
  (glki-stream-open-memory buf buflen fmode rock stream-id t))

(defun glki-stream-open-file (fileref fmode rock stream-id unicode)
  (let ((stream (glki-opq-stream-create stream-id))
        (filename (glki-opq-fileref-get-filename fileref))
        (find-file-hook nil))
    (glki-opq-stream-set-buffer stream
                                (cond ((and (eq 'glk-filemode-read fmode) (not (file-exists-p filename)))
                                       (signal 'glk-error (list "Can't read from a non-existant file" filename)))
                                      ((eq 'glk-filemode-write fmode) (generate-new-buffer "*glk*"))
                                      ((eq 'glk-filemode-read fmode) (find-file-noselect filename t))))
    (glki-opq-stream-set-type stream 'glki-file-stream)
    (glki-opq-stream-set-read-count stream 0)
    (glki-opq-stream-set-write-count stream 0)
    (glki-opq-stream-set-storage stream fileref)
    (glki-opq-stream-set-unicode stream unicode)
    (glki-opq-stream-set-mode stream fmode)
    (glki-opq-stream-set-rock stream rock)
    stream))

(defun glk-stream-open-file (fileref fmode rock stream-id)
  (glki-stream-open-file fileref fmode rock stream-id nil))

(defun glk-stream-open-file-uni (fileref fmode rock stream-id)
  (glki-stream-open-file fileref fmode rock stream-id t))

(defun glki-stream-dispose (stream)
  (if (glki-opq-stream-get-buffer stream)
      (kill-buffer (glki-opq-stream-get-buffer stream)))
  (glki-opq-stream-dispose stream))

(defun glki-kill-all-streams ()
  "Cleans up all glk streams and destroys the associated buffers"
  (mapcar #'glki-stream-dispose glki-opq-stream))

(defun glk-stream-close (stream)
  (let ((type (glki-opq-stream-get-type stream)))
    (when (eq type 'glki-window-stream)
      (signal 'glk-error (list "Can't close a window stream" stream)))
    (let ((result (cond ((eq type 'glki-memory-stream)
                         (list nil (list (eq (glki-opq-stream-get-type stream) 'glki-memory-stream)
                                         (glki-opq-stream-get-read-count stream)
                                         (glki-opq-stream-get-write-count stream)
                                         (glki-opq-stream-get-unicode stream)
                                         (glki-opq-stream-get-storage stream)
                                         (with-current-buffer (glki-opq-stream-get-buffer stream)
                                           (buffer-string)))))
                        ((eq type 'glki-file-stream)
                         (with-current-buffer (glki-opq-stream-get-buffer stream)
                           (write-file (glki-opq-fileref-get-filename (glki-opq-stream-get-storage stream))))
                         (list nil (list nil))))))
      (glki-stream-dispose stream)
      result)))

(defun glk-set-style-stream (str val)
  (glki-opq-stream-set-face str val)
  nil)

(defun glk-set-style (val)
  (glk-set-style-stream glk-current-stream val)
  nil)

(provide 'glk-stream)
