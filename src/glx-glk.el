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

(puthash #x04 (list #'glk-gestalt
                    (list #'glx-32->glk-gestalt-selector 0)
                    (list #'glx-32->int 1)) *glx-glk-functions*)

(puthash #x20 (list #'glk-window-iterate
                    (list #'glx-32->glk-opq 0)
                    (list (list 1) (list 1) #'glx-store-glk-result)) *glx-glk-functions*)

(puthash #x23 (list #'glk-window-open
                    (list #'glx-32->glk-opq 0)
                    (list #'glx-32->glk-winmethod 1)
                    (list #'glx-32->int 2)
                    (list #'glx-32->glk-wintype 3)
                    (list #'glx-32->int 4)
                    (list #'glx-get-next-glk-id)
                    (list #'glx-get-next-glk-id)
                    (list #'glx-get-next-glk-id)
                    (list #'glx-get-next-glk-id)) *glx-glk-functions*)

(puthash #x25 (list #'glk-window-get-size
                    (list #'glx-32->glk-opq 0)
                    (list (list 1) (list 1) #'glx-store-glk-result)
                    (list (list 2) (list 2) #'glx-store-glk-result)) *glx-glk-functions*)

(puthash #x2a (list #'glk-window-clear
                    (list #'glx-32->glk-opq 0)) *glx-glk-functions*)

(puthash #x2b (list (lambda (a b c) nil)
                    (list #'identity 0)
                    (list #'identity 1)
                    (list #'identity 2)) *glx-glk-functions*)

(puthash #x2f (list #'glk-set-window
                    (list #'glx-32->glk-opq 0)) *glx-glk-functions*)

(puthash #x40 (list #'glk-stream-iterate
                    (list #'glx-32->glk-opq 0)
                    (list (list 1) (list 1) #'glx-store-glk-result)) *glx-glk-functions*)

(puthash #x43 (list #'glk-stream-open-memory
                    (list #'identity 0)
                    (list #'glx-32->int 1)
                    (list #'glx-32->int 2)
                    (list #'glx-32->int 3)
                    (list #'glx-get-next-glk-id)) *glx-glk-functions*)

(puthash #x44 (list #'glk-stream-close
                    (list #'glx-32->glk-opq 0)
                    (list (list 1) (list 1) #'glx-glk-store-closed-memory-stream)) *glx-glk-functions*)

(puthash #x47 (list #'glk-stream-set-current
                    (list #'glx-32->glk-opq 0)) *glx-glk-functions*)

(puthash #x48 (list #'glk-stream-get-current) *glx-glk-functions*)

(puthash #x64 (list #'glk-fileref-iterate
                    (list #'glx-32->glk-opq 0)
                    (list (list 1) (list 1) #'glx-store-glk-result)) *glx-glk-functions*)

(puthash #x80 (list #'glk-put-char
                    (list #'glx-32->int 0)) *glx-glk-functions*)

(puthash #x86 (list #'glk-set-style
                    (list #'glx-32->glk-style 0)) *glx-glk-functions*)

(puthash #xa0 (list #'glk-char-to-lower
                    (list #'glx-32->int 0)) *glx-glk-functions*)

(puthash #xb0 (list (lambda (a b c d) nil)
                    (list #'identity 0)
                    (list #'identity 1)
                    (list #'identity 2)
                    (list #'identity 3)) *glx-glk-functions*)

(puthash #xc0 (list #'glx-glk-select
                    (list #'identity 0)) *glx-glk-functions*)

(puthash #xc1 (list #'glx-glk-select-poll
                    (list #'identity 0)) *glx-glk-functions*)

(puthash #xd0 (list #'glk-request-line-event
                    (list #'glx-32->glk-opq 0)
                    (list #'identity 1)
                    (list #'glx-32->int 2)
                    (list #'glx-32->int 3)) *glx-glk-functions*)

(puthash #x128 (list #'glk-put-char
                     (list #'glx-32->int 0)) *glx-glk-functions*)

(puthash #x139 (list #'glk-stream-open-memory-uni
                     (list #'identity 0)
                     (list #'glx-32->int 1)
                     (list #'glx-32->int 2)
                     (list #'glx-32->int 3)
                     (list #'glx-get-next-glk-id)) *glx-glk-functions*)

(defun glx-get-next-glk-id ()
  (incf *glx-glk-id-gen*)
  (intern (prin1-to-string (glx-32 *glx-glk-id-gen*))))

(defun glx-glk-args-from-stack (arg-count)
  (let ((args (make-vector arg-count nil)))
    (dotimes (n arg-count args)
      (aset args n (glx-value-pop)))))

(defun glx-glk-select-args-for-marshalling (all-args positions)
  (mapcar (lambda (p) (aref all-args p)) positions))

(defun glx-glk-call (selector arg-count)
  "Call the glk function with the given SELECTOR. There should
be ARG-COUNT args on the stack."

  (let ((glk-fun (gethash selector *glx-glk-functions*))
        (all-args (glx-glk-args-from-stack arg-count))
        args
        stores)
    (glx-log "looking for glk function %x" selector)
    (when (null glk-fun) (signal 'glx-glk-error (list "Can't find glk function" selector)))
    (glx-log "found glk function %s - marshalling %s " glk-fun (cdr glk-fun))

    (dolist (marshall (cdr glk-fun))
      (if (functionp (car marshall))
          (push (apply (car marshall) (glx-glk-select-args-for-marshalling all-args (cdr marshall))) args)
        (push (cons (glx-glk-select-args-for-marshalling all-args (car marshall)) (cdr marshall)) stores)))
    
    (glx-log "Calling glk function %x with args %S - stores %S" selector (reverse args) stores)
    (glx-handle-glk-results (apply (car glk-fun) (reverse args)) stores)))

(defun glx-handle-glk-results (results stores)
  "Each store in STORES consists of a function, list of positions,
a list of arguments from the glk call and a function.
The specified results (given by the positions) from the RESULTS list, 
and the values picked from the original glk call arguments, are all
passed to the function for processing. The first
result from the RESULTS list is used as the result of the
entire glk call."
  (dolist (store stores)
    (let ((store-function-args (car store))
          (positions (cadr store))
          (store-function (caddr store)))

      (dolist (p positions)
        (push (nth p results) store-function-args))

      (glx-log "Storing glk result %S" store-function-args)
      (apply store-function store-function-args)))
  
  (glx-log "Last glk return value %S" (glx-glk-result->32 results))
  (glx-get-glk-result results))

(defun glx-32->glk-opq (value)
  (if (glx-0-p value) nil (intern (prin1-to-string value))))

(defconst glk-winmethod-position-decode
  '((3 glk-winmethod-below) (2 glk-winmethod-above) (1 glk-winmethod-right) (0 glk-winmethod-left)))

(defun glki-winmethod (winmethod)
  (list
   (cadr (assoc (logand winmethod #xf) glk-winmethod-position-decode))
   (if (>= winmethod #x20)
       'glk-winmethod-proportional
     'glk-winmethod-fixed)))

(defun glx-32->glk-winmethod (value)
  (glki-winmethod (glx-32->int value)))

(defun glx-32->glk-wintype (value)
  (case (glx-32->int value)
    (0 'glk-wintype-all-types)
    (1 'glk-wintype-pair)
    (2 'glk-wintype-blank)
    (3 'glk-wintype-text-buffer)
    (4 'glk-wintype-text-grid)
    (5 'glk-wintype-graphics)))

(defun glx-32->glk-style (value)
  (case (glx-32->int value)
    (1 'glk-emphasizes-face)
    (3 'glk-header-face)
    (4 'glk-subheader-face)
    (t 'glk-normal-face)))

(defun glx-32->glk-gestalt-selector (value)
  (case (glx-32->int value)
    (0 'glk-gestalt-version)))

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
      (glx-log "Storing event at %s : %s" event-memptr result)
      (glx-glk-store-event result event-memptr)
      glx-0)))

(defun glx-glk-select-poll (event-memptr)
  (glx-glk-store-event (list 'glk-evtype-none
                             (glx-32->glk-opq glx-0)
                             0
                             0) event-memptr))

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

(defun glx-glk-event-type->int (event-type)
  (cond
   ((eq event-type 'glk-evtype-none) 0)
   ((eq event-type 'glk-evtype-lineinput) 3)))


(defun glx-glk-store-event (event memptr)
  (glx-log "storing event to location: %S - %S" memptr event)
  (glx-store-glk-structure memptr
                           (list (glx-32 (glx-glk-event-type->int (first event)))
                                 (glx-glk-opq->glx-32 (second event))
                                 (glx-32 (third event))
                                 (glx-32 (fourth event))))
  (when (eq (first event) 'glk-evtype-lineinput)
    (glx-memory-set-string (fifth event) (sixth event))))

(defun glx-memory-set-unicode-string (memptr string)
  (mapcar (lambda (c) (glx-memory-set memptr (glx-32 c) 4) (setq memptr (glx-+ glx-4 memptr))) string))

(defun glx-glk-store-closed-memory-stream (memptr stream)
  (glx-log "storing stream to location: %S - %S" memptr stream)
  (glx-store-glk-structure memptr
                           (list (glx-32 (first stream))
                                 (glx-32 (second stream))))
  (if (third stream)
      (glx-memory-set-unicode-string (fourth stream) (fifth stream))
    (glx-memory-set-string (fourth stream) (fifth stream))))

(provide 'glx-glk)

