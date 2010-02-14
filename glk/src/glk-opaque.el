(eval-when-compile
  (require 'cl))

(defun glki-get-rock (object)
  (get object 'rock))

(defun glki-iterate-get-next (collection current)
  (cond
   ((null collection) nil)
   ((null current) (list (car collection) (glki-get-rock (car collection))))
   ((eq (car collection) current) (glki-iterate-get-next (cdr collection) nil))
   (t (glki-iterate-get-next (cdr collection) current))))

(defmacro defopaque (name &rest slots)
  (let ((name-string (symbol-name name)))
	(flet ((make-symbol (prefix &optional suffix) (intern (concat prefix name-string suffix))))
	  (let ((arg-name (make-symbol "glk-"))
                (id-name (make-symbol "glk-" "-id"))
                (slots (cons 'rock slots)))
            `(progn

               (defvar ,(make-symbol "glki-opq-") nil ,(concat "The collection of glk " name-string " opaque objects"))

               (defun ,(make-symbol "glki-opq-clear-" "-plist") (,arg-name)
                 ,@(mapcar (lambda (slot-name) `(put ,arg-name (quote ,slot-name) nil))
                           slots))

               ,@(mapcar (lambda (slot-name)
                           (let ((slot-string (symbol-name slot-name)))
                             `(defun ,(intern (concat "glki-opq-" name-string "-set-" slot-string))
                                (,arg-name value)
                                ,(concat "Sets the " slot-string " value of this " name-string)
                                (put ,arg-name ',slot-name value))))
                         slots)

               ,@(mapcar (lambda (slot-name)
                           (let ((slot-string (symbol-name slot-name)))
                             `(defun ,(intern (concat "glki-opq-" name-string "-get-" slot-string))
                                (,arg-name)
                                ,(concat "Gets the " slot-string " value of this " name-string)
                                (get ,arg-name ',slot-name))))
                         slots)

               (defun ,(make-symbol "glk-" "-iterate") (,arg-name)
                 ,(concat "Iterates through the instances of " name-string ". Returns the next " name-string " after the given one.")
                 (glki-iterate-get-next ,(make-symbol "glki-opq-") ,arg-name))

               (defun ,(make-symbol "glki-opq-" "-create") (,id-name)
                 ,(concat "Creates and stores a new instance of " name-string)
                 (when ,id-name
                     (,(make-symbol "glki-opq-clear-" "-plist") ,id-name)
                     (push ,id-name ,(make-symbol "glki-opq-")))
                 ,id-name)

               (defun ,(make-symbol "glki-opq-" "-dispose") (,id-name)
                 ,(concat "Disposes of an instance of " name-string)
                 (,(make-symbol "glki-opq-clear-" "-plist") ,id-name)
                 (setq ,(make-symbol "glki-opq-") (remove ,id-name ,(make-symbol "glki-opq-")))))))))

(provide 'glk-opaque)
