(require 'glk-opaque)

(defvar glk-current-stream nil
  "The current stream for reading and writing.
Used by routines which do not specify a stream")

(defopaque stream buffer type read-count write-count storage face)

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
                'face (glki-get-current-style-face)
                'hard t)))

(defun glki-get-current-style-face ()
  (glki-opq-stream-get-face glk-current-stream))

(defun glki-post-process-current-buffer (stream pre-insert-point)
  (unless (eq (glki-opq-stream-get-type stream) 'glki-memory-stream)
    (unless (string= " " (buffer-substring (- (line-end-position) 1) (line-end-position)))
      (fill-region pre-insert-point (line-end-position)))))

(defun glk-put-string-stream (str s)
  (save-current-buffer
    (set-buffer (glki-opq-stream-get-buffer str))
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
  (save-current-buffer
    (set-buffer (glki-opq-stream-get-buffer str))
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

(defun glk-stream-open-memory (buf buflen fmode rock stream-id)
  (let ((stream (glki-opq-stream-create stream-id)))
    (glki-opq-stream-set-buffer stream (generate-new-buffer "*glk*"))
    (glki-opq-stream-set-type stream 'glki-memory-stream)
    (glki-opq-stream-set-read-count stream 0)
    (glki-opq-stream-set-write-count stream 0)
    (glki-opq-stream-set-storage stream buf)
    stream))

(defun glki-stream-dispose (stream)
  (if (glki-opq-stream-get-buffer stream)
      (kill-buffer (glki-opq-stream-get-buffer stream)))
  (glki-opq-stream-dispose stream))

(defun glki-kill-all-streams ()
  "Cleans up all glk streams and destroys the associated buffers"
  (mapcar #'glki-stream-dispose glki-opq-stream))

(defun glk-stream-close (stream)
  (when (eq (glki-opq-stream-get-type stream) 'glki-window-stream)
    (signal 'glk-error (list "Can't close a window stream" stream)))
  (let ((result (list nil (list (glki-opq-stream-get-read-count stream)
                                (glki-opq-stream-get-write-count stream)
                                (glki-opq-stream-get-storage stream)
                                (save-current-buffer
                                  (set-buffer (glki-opq-stream-get-buffer stream))
                                  (buffer-string))))))
    (glki-stream-dispose stream)
    result))

(defun glk-set-style-stream (str val)
  (glki-opq-stream-set-face str val)
  nil)

(defun glk-set-style (val)
  (glk-set-style-stream glk-current-stream val)
  nil)

(provide 'glk-stream)