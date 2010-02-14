(require 'glk-opaque)

(defopaque fileref filename)

(defun glk-fileref-create-by-name (usage name rock new-fileref)
  (glki-opq-fileref-create new-fileref)
  (glki-opq-fileref-set-filename new-fileref name)
  new-fileref)

(defun glk-fileref-create-by-prompt (usage fmode rock new-fileref)
  (glk-fileref-create-by-name usage (read-from-minibuffer "file name") rock new-fileref))

(defun glk-fileref-destroy (fref)
  (glki-opq-fileref-dispose fref))

(defun glki-get-filename (fileref)
  "Get the filename referred to by this fileref"
  (glki-opq-fileref-get-filename fileref))

(provide 'glk-file)
