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

(require 'cl-lib)

(defun glki-iterate-get-next (collection rock-fn current)
  (when collection
    (let ((result (unless current (car collection))))
      (while (and (not result) collection)
        (when (eq current (cdar collection))
          (setq result (cadr collection)))
        (setq collection (cdr collection)))
      (when result
        (list (cdr result) (funcall rock-fn (cdr result)))))))

(defmacro defopaque (name &rest slots)
  (let ((name-string (symbol-name name)))
    (cl-labels ((make-symbol (prefix &optional suffix) (intern (concat prefix name-string suffix))))
      (let* ((arg-name (make-symbol "glk-"))
             (id-name (make-symbol "glk-" "-id"))
             (structure-name (make-symbol "glki-opq-"))
             (collection-name (make-symbol "glki-opq-"))
             (private-create-name (make-symbol "glki-opq-" "-private-create"))
             (slots (cons id-name (cons 'rock slots)))
             (keyword-slots (cl-mapcan (lambda (s) (list (intern (concat ":" (symbol-name s))) s)) slots)))
        `(progn
           
           (defvar ,collection-name nil ,(concat "The collection of glk " name-string " opaque objects"))

           (cl-defstruct (,structure-name (:constructor ,private-create-name)
                                          (:copier nil))
             ,@slots)
           
           (defun ,(make-symbol "glk-" "-iterate") (,arg-name)
             ,(concat "Iterates through the instances of " name-string ". Returns the next " name-string " after the given one.")
             (glki-iterate-get-next ,collection-name #',(make-symbol "glki-opq-" "-rock") ,arg-name))
           
           (defun ,(make-symbol "glki-opq-" "-create") ,slots
             ,(concat "Creates and stores a new instance of " name-string)
             (when ,id-name
               (let ((new-object (,private-create-name ,@keyword-slots)))
                 (push (cons ,id-name new-object) ,collection-name)
                 new-object)))
           
           (defun ,(make-symbol "glki-opq-" "-dispose") (,arg-name)
             ,(concat "Disposes of an instance of " name-string)
             (setq ,collection-name (rassq-delete-all ,arg-name ,collection-name)))

           (defun ,(make-symbol "glki-opq-" "-lookup") (,id-name)
             ,(concat "Lookup an instance of " name-string " from its id")
             (cdr (assq ,id-name ,collection-name))))))))

(provide 'glk-opaque)
