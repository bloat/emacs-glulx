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

(require 'glx-value)
(require 'glx-glulx)
(require 'glx-stack)
(require 'glx-load)
(require 'glx-exec)

(put 'glx-glk-error 'error-conditions '(error glx-error glx-glk-error))
(put 'glx-glk-error 'error-message "Glulx VM glk error")

(defconst *glx-glk-functions* (make-hash-table))

(puthash #x04 (list #'glk-gestalt #'glx-32->glk-gestalt-selector #'glx-32->int) *glx-glk-functions*)
(puthash #x20 (list #'glk-window-iterate #'glx-32->glk-opq (list 1 #'glx-store-glk-result)) *glx-glk-functions*)
(puthash #x23 (list #'glk-window-open
                    #'glx-32->glk-opq
                    #'glx-32->glk-winmethod
                    #'glx-32->int
                    #'glx-32->glk-wintype
                    #'glx-32->int
                    'gen-id 'gen-id 'gen-id 'gen-id) *glx-glk-functions*)
(puthash #x25 (list #'glk-window-get-size
                    #'glx-32->glk-opq
                    (list 1 #'glx-store-glk-result)
                    (list 2 #'glx-store-glk-result)) *glx-glk-functions*)
(puthash #x2a (list #'glk-window-clear #'glx-32->glk-opq) *glx-glk-functions*)
(puthash #x2b (list (lambda (a b c) nil) #'identity #'identity #'identity) *glx-glk-functions*)
(puthash #x2f (list #'glk-set-window #'glx-32->glk-opq) *glx-glk-functions*)
(puthash #x40 (list #'glk-stream-iterate #'glx-32->glk-opq (list 1 #'glx-store-glk-result)) *glx-glk-functions*)
(puthash #x43 (list #'glk-stream-open-memory #'identity #'glx-32->int #'glx-32->int #'glx-32->int 'gen-id) *glx-glk-functions*)
(puthash #x44 (list #'glk-stream-close #'glx-32->glk-opq (list 1 #'glx-glk-store-closed-memory-stream)) *glx-glk-functions*)
(puthash #x47 (list #'glk-stream-set-current #'glx-32->glk-opq) *glx-glk-functions*)
(puthash #x48 (list #'glk-stream-get-current) *glx-glk-functions*)
(puthash #x64 (list #'glk-fileref-iterate #'glx-32->glk-opq (list 1 #'glx-store-glk-result)) *glx-glk-functions*)
(puthash #x86 (list #'glk-set-style #'glx-32->glk-style) *glx-glk-functions*)
(puthash #xa0 (list #'glk-char-to-lower #'glx-32->int) *glx-glk-functions*)
(puthash #xb0 (list (lambda (a b c d) nil) #'identity #'identity #'identity #'identity) *glx-glk-functions*)
(puthash #xc0 (list #'glx-glk-select #'identity) *glx-glk-functions*)
(puthash #xd0 (list #'glk-request-line-event
                    #'glx-32->glk-opq #'identity
                    #'glx-32->int #'glx-32->int) *glx-glk-functions*)

(defun glx-get-next-glk-id ()
  (incf *glx-glk-id-gen*)
  (intern (prin1-to-string (glx-32 *glx-glk-id-gen*))))

(defun glx-glk-call (selector arg-count)
  "Call the glk function with the given SELECTOR. There should
be ARG-COUNT args on the stack."
  (let ((glk-fun (gethash selector *glx-glk-functions*))
        args
        stores)
    (glx-log "looking for glk function %x" selector)
    (when (null glk-fun) (signal 'glx-glk-error (list "Can't find glk function" selector)))
    (when (not (= (length (remove-if (lambda (x) (eq x 'gen-id)) (cdr glk-fun))) arg-count))
      (signal 'glx-glk-error (list "Glk arity mismatch: function expects" (length (cdr glk-fun)) "received" arg-count)))
    (glx-log "found glk function %s - marshalling %s " glk-fun (cdr glk-fun))
    (dolist (marshall (cdr glk-fun))
      (cond ((functionp marshall)
             (push (funcall marshall (glx-value-pop)) args))
            ((eq 'gen-id marshall) (push (glx-get-next-glk-id) args))
            (t (push (cons marshall (glx-value-pop)) stores))))
    (glx-log "Calling glk function %x with args %S - stores %S" selector (reverse args) stores)
    (glx-handle-glk-results (apply (car glk-fun) (reverse args)) stores)))

(defun glx-handle-glk-results (results stores)
  "Each store in STORES consists of a position and a function.
The nth result (given by the position] from the RESULTS list is
picked out and passed to the function for processing. The first
result from the RESULTS list is used as the result of the
entire glk call."
  (dolist (store stores)
    (let ((result-position (first (car store)))
          (store-function (second (car store))))
      (let ((result (glx-glk-result->32 (nth result-position results))))
        (glx-log "Storing glk result %S to %S" result store)
        (funcall store-function (cdr store) result))))
  (glx-log "Last glk return value %S" (glx-glk-result->32 results))
  (glx-get-glk-result results))

(defun glx-32->glk-opq (value)
  (if (glx-0-p value) nil (intern (prin1-to-string value))))

(defun glx-32->glk-winmethod (value)
  (glki-winmethod (glx-32->int value)))

(defun glx-32->glk-wintype (value)
  (glki-wintype (glx-32->int value)))

(defun glx-32->glk-style (value)
  (glki-style (glx-32->int value)))

(defun glx-32->glk-gestalt-selector (value)
  (glki-gestalt-selector (glx-32->int value)))

(defun glx-glk-opq->glx-32 (value)
  (read (symbol-name value)))

(defun glx-glk-result->32 (result)
  (cond ((null result) glx-0)
        ((symbolp result) (glx-glk-opq->glx-32 result))
        ((numberp result) (glx-32 result))
        ((listp result) result)
        (t (signal 'glx-glk-error (list "Unknown glk result type:" result)))))

(defun glx-get-glk-result (result)
  "The RESULT of a glk function may be a list or a scalar value.
If it is a list the the car of the list should be the result of the
glk call."
  (glx-glk-result->32 (if (listp result) (car result) result)))

(defun glx-glk-select (event-memptr)
  (let ((result (glk-select)))
    (if (eq result 'glk-no-return)
        (progn
          (setq *glx-store-event-memptr* event-memptr)
          (setq *glx-unexpected-exit* nil)
          'glk-no-return)
      (glx-glk-store-event result event-memptr)
      glx-0)))

(defun glx-glk-event-callback (event)
  (setq *glx-unexpected-exit* t)
  (glx-glk-store-event event *glx-store-event-memptr*)
  (unwind-protect
      (progn
        (while (glx-execute-next-instruction)))
    (glx-cleanup)))

(defun glx-store-glk-result (memptr result)
  (glx-store-glk-structure memptr (list result)))

(defun glx-store-glk-structure (memptr struct)
  "Stores a glk structure i.e. a list of 32 bit values into memory.
If the memory address is -1 then the values are pushed onto the stack, last topmost.
If the memory address is 0 then all results are discarded."
  (unless (glx-0-p memptr)
    (dolist (val struct)
      (if (equal memptr (glx-32 -1))
          (glx-value-push val)
        (glx-memory-set memptr val 4)
        (setf memptr (glx-+ glx-4 memptr))))))

(defun glx-glk-store-event (event memptr)
  (glx-log "storing event to location: %S - %S" memptr event)
  (glx-store-glk-structure memptr
                           (list (glx-32 (glki-event-type->int (first event)))
                                 (glx-glk-opq->glx-32 (second event))
                                 (glx-32 (third event))
                                 (glx-32 (fourth event))))
  (glx-memory-set-string (fifth event) (sixth event)))

(defun glx-glk-store-closed-memory-stream (memptr stream)
  (glx-log "storing stream to location: %S - %S" memptr stream)
  (glx-store-glk-structure memptr
                           (list (glx-32 (first stream))
                                 (glx-32 (second stream))))
  (glx-memory-set-string (third stream) (fourth stream)))

(provide 'glx-glk)
