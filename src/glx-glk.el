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

(require 'glx-value)
(require 'glx-glulx)
(require 'glx-stack)
(require 'glx-load)
(require 'glx-exec)
(require 'glk)

(put 'glx-glk-error 'error-conditions '(error glx-error glx-glk-error))
(put 'glx-glk-error 'error-message "Glulx VM glk error")

(defconst *glx-glk-functions* (make-hash-table))

;; Each glk function in the hash table consists of a list of
;; a) the elisp function to call
;; b) the instructions for marshalling the arguments and return values.
;;
;; A marshalling instruction is a list, the first item is either a
;; function or another list.
;;
;; If the first item is a function then we are marshalling one of the
;; input arguments to the glk function. Using the positions given by
;; the other items in the marshalling instruction we select items from
;; the arguments to the glk call, and the marshalling function is
;; called one those items, to create one input argument for the glk
;; function.
;;
;; If the first item is a list then we are marshalling some output
;; from the glk function. The first item in the list is a list of
;; positions from the input arguments. The second item in the list is
;; a list of positions from the results. The values specified by these
;; lists are concatenated and passed to the function which is the
;; third item in the marshalling instruction.

(puthash #x04 (list #'glk-gestalt
                    (list #'glx-32->glk-gestalt-selector 0)
                    (list #'glx-32->int 1)) *glx-glk-functions*)

(puthash #x20 (list #'glk-window-iterate
                    (list #'glx-32->glk-window 0)
                    (list (list 1) (list 1) #'glx-store-glk-result)) *glx-glk-functions*)

(puthash #x23 (list #'glk-window-open
                    (list #'glx-32->glk-window 0)
                    (list #'glx-32->glk-winmethod 1)
                    (list #'glx-32->int 2)
                    (list #'glx-32->glk-wintype 3)
                    (list #'glx-32->int 4)
                    (list #'glx-get-next-glk-id)
                    (list #'glx-get-next-glk-id)
                    (list #'glx-get-next-glk-id)
                    (list #'glx-get-next-glk-id)) *glx-glk-functions*)

(puthash #x25 (list #'glk-window-get-size
                    (list #'glx-32->glk-window 0)
                    (list (list 1) (list 1) #'glx-store-glk-result)
                    (list (list 2) (list 2) #'glx-store-glk-result)) *glx-glk-functions*)

(puthash #x29 (list #'glk-window-get-parent
                    (list #'glx-32->glk-window 0)) *glx-glk-functions*)

(puthash #x2a (list #'glk-window-clear
                    (list #'glx-32->glk-window 0)) *glx-glk-functions*)

(puthash #x2b (list #'glk-window-move-cursor
                    (list #'glx-32->glk-window 0)
                    (list #'glx-32->int 1)
                    (list #'glx-32->int 2)) *glx-glk-functions*)

(puthash #x2f (list #'glk-set-window
                    (list #'glx-32->glk-window 0)) *glx-glk-functions*)

(puthash #x30 (list #'glk-window-get-sibling
                    (list #'glx-32->glk-window 0)) *glx-glk-functions*)

(puthash #x40 (list #'glk-stream-iterate
                    (list #'glx-32->glk-stream 0)
                    (list (list 1) (list 1) #'glx-store-glk-result)) *glx-glk-functions*)

(puthash #x42 (list #'glk-stream-open-file
                    (list #'glx-32->glk-fileref 0)
                    (list #'glx-32->glk-filemode 1)
                    (list #'glx-32->int 2)
                    (list #'glx-get-next-glk-id)) *glx-glk-functions*)

(puthash #x43 (list #'glk-stream-open-memory
                    (list #'identity 0)
                    (list #'glx-32->int 1)
                    (list #'glx-32->glk-filemode 2)
                    (list #'glx-32->int 3)
                    (list #'glx-get-next-glk-id)) *glx-glk-functions*)

(puthash #x44 (list #'glk-stream-close
                    (list #'glx-32->glk-stream 0)
                    (list (list 1) (list 1) #'glx-glk-store-closed-stream)) *glx-glk-functions*)

(puthash #x47 (list #'glk-stream-set-current
                    (list #'glx-32->glk-stream 0)) *glx-glk-functions*)

(puthash #x48 (list #'glk-stream-get-current) *glx-glk-functions*)

(puthash #x60 (list #'glk-fileref-create-temp
                    (list #'glx-32->int 0)
                    (list #'glx-32->int 1)
                    (list #'glx-get-next-glk-id)) *glx-glk-functions*)

(puthash #x61 (list #'glk-fileref-create-by-name
                    (list #'glx-32->int 0)
                    (list #'glx-glk-load-string 1)
                    (list #'glx-32->int 2)
                    (list #'glx-get-next-glk-id)) *glx-glk-functions*)

(puthash #x63 (list #'glk-fileref-destroy
                    (list #'glx-32->glk-fileref 0)) *glx-glk-functions*)

(puthash #x64 (list #'glk-fileref-iterate
                    (list #'glx-32->glk-fileref 0)
                    (list (list 1) (list 1) #'glx-store-glk-result)) *glx-glk-functions*)

(puthash #x66 (list #'glk-fileref-delete-file
                    (list #'glx-32->glk-fileref 0)) *glx-glk-functions*)

(puthash #x67 (list #'glk-fileref-does-file-exist
                    (list #'glx-32->glk-fileref 0)) *glx-glk-functions*)

(puthash #x80 (list #'glk-put-char
                    (list #'glx-32->int 0)) *glx-glk-functions*)

(puthash #x81 (list #'glk-put-char-stream
                    (list #'glx-32->glk-stream 0)
                    (list #'glx-32->int 1)) *glx-glk-functions*)

(puthash #x82 (list #'glk-put-string
                    (list #'glx-glk-load-string 0)) *glx-glk-functions*)

(puthash #x83 (list #'glk-put-string-stream
                    (list #'glx-32->glk-stream 0)
                    (list #'glx-glk-load-string 1)) *glx-glk-functions*)

(puthash #x84 (list #'glk-put-string
                    (list #'glx-glk-load-string-buffer 0 1)) *glx-glk-functions*)

(puthash #x85 (list #'glk-put-string-stream
                    (list #'glx-32->glk-stream 0)
                    (list #'glx-glk-load-string-buffer 1 2)) *glx-glk-functions*)

(puthash #x86 (list #'glk-set-style
                    (list #'glx-32->glk-style 0)) *glx-glk-functions*)

(puthash #x92 (list #'glk-get-buffer-stream
                    (list #'glx-32->glk-stream 0)
                    (list #'glx-32->int 2)
                    (list (list 1) (list 1) #'glx-glk-store-string)) *glx-glk-functions*)

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
                    (list #'glx-32->glk-window 0)
                    (list #'identity 1)
                    (list #'glx-32->int 2)
                    (list #'glx-32->int 3)) *glx-glk-functions*)

(puthash #xd2 (list #'glk-request-char-event
                    (list #'glx-32->glk-window 0)) *glx-glk-functions*)

(puthash #x120 (list #'glk-buffer-to-lower-case-uni
                     (list #'glx-glk-load-unicode-string-buffer 0 2)
                     (list (list 0 1) (list 1) #'glx-glk-store-unicode-string-buffer)) *glx-glk-functions*)

(puthash #x121 (list #'glk-buffer-to-upper-case-uni
                     (list #'glx-glk-load-unicode-string-buffer 0 2)
                     (list (list 0 1) (list 1) #'glx-glk-store-unicode-string-buffer)) *glx-glk-functions*)

(puthash #x122 (list #'glk-buffer-to-title-case-uni
                     (list #'glx-glk-load-unicode-string-buffer 0 2)
                     (list #'glx-32->glk-boolean 3)
                     (list (list 0 1) (list 1) #'glx-glk-store-unicode-string-buffer)) *glx-glk-functions*)

(puthash #x128 (list #'glk-put-char
                     (list #'glx-32->int 0)) *glx-glk-functions*)

(puthash #x129 (list #'glk-put-string
                     (list #'glx-glk-load-string-uni 0)) *glx-glk-functions*)

(puthash #x12a (list #'glk-put-string
                     (list #'glx-glk-load-unicode-string-buffer 0 1)) *glx-glk-functions*)

(puthash #x12b (list #'glk-put-char-stream
                     (list #'glx-32->glk-stream 0)
                     (list #'glx-32->int 1)) *glx-glk-functions*)

(puthash #x12c (list #'glk-put-string-stream
                     (list #'glx-32->glk-stream 0)
                     (list #'glx-glk-load-string-uni 1)) *glx-glk-functions*)

(puthash #x12d (list #'glk-put-string-stream
                     (list #'glx-32->glk-stream 0)
                     (list #'glx-glk-load-unicode-string-buffer 1 2)) *glx-glk-functions*)

(puthash #x130 (list #'glk-get-char-stream
                     (list #'glx-32->glk-stream 0)) *glx-glk-functions*)

(puthash #x131 (list #'glk-get-buffer-stream
                    (list #'glx-32->glk-stream 0)
                    (list #'glx-32->int 2)
                    (list (list 1) (list 1) #'glx-glk-store-string-uni)) *glx-glk-functions*)

(puthash #x138 (list #'glk-stream-open-file-uni
                     (list #'glx-32->glk-fileref 0)
                     (list #'glx-32->glk-filemode 1)
                     (list #'glx-32->int 2)
                     (list #'glx-get-next-glk-id)) *glx-glk-functions*)

(puthash #x139 (list #'glk-stream-open-memory-uni
                     (list #'identity 0)
                     (list #'glx-32->int 1)
                     (list #'glx-32->glk-filemode 2)
                     (list #'glx-32->int 3)
                     (list #'glx-get-next-glk-id)) *glx-glk-functions*)

(defun glx-get-next-glk-id ()
  (cl-incf *glx-glk-id-gen*)
  (intern (prin1-to-string (glx-32-get-bytes-as-list-big-endian (glx-32 *glx-glk-id-gen*)))))

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
and a list of arguments from the glk call.
The specified results (given by the positions) from the RESULTS list, 
and the values picked from the original glk call arguments, are all
passed to the function for processing. The first
result from the RESULTS list is used as the result of the
entire glk call."
  (dolist (store stores)
    (let ((store-function-args (car store))
          (positions (cadr store))
          (store-function (cl-caddr store)))

      (dolist (p positions)
        (push (nth p results) store-function-args))

      (glx-log "Storing glk result %S" store-function-args)
      (apply store-function store-function-args)))

  (let ((glk-call-result (glx-get-glk-result results)))
    (glx-log "Last glk return value %S" glk-call-result)
    glk-call-result))

(defun glx-32->glk-stream (value)
  (if (glx-0-p value) nil (glki-opq-stream-lookup (intern (prin1-to-string (glx-32-get-bytes-as-list-big-endian value))))))

(defun glx-32->glk-window (value)
  (if (glx-0-p value) nil (glki-opq-window-lookup (intern (prin1-to-string (glx-32-get-bytes-as-list-big-endian value))))))

(defun glx-32->glk-fileref (value)
  (if (glx-0-p value) nil (glki-opq-fileref-lookup (intern (prin1-to-string (glx-32-get-bytes-as-list-big-endian value))))))

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
  (cond
   ((= value 0) 'glk-wintype-all-types)
   ((= value 1) 'glk-wintype-pair)
   ((= value 2) 'glk-wintype-blank)
   ((= value 3) 'glk-wintype-text-buffer)
   ((= value 4) 'glk-wintype-text-grid)
   ((= value 5) 'glk-wintype-graphics)))

(defun glx-32->glk-style (value)
  (cond
   ((= value 1) 'glk-emphasizes-face)
   ((= value 3) 'glk-header-face)
   ((= value 4) 'glk-subheader-face)
   ('glk-normal-face)))

(defun glx-32->glk-filemode (value)
  (cond
   ((= value 1) 'glk-filemode-write)
   ((= value 2) 'glk-filemode-read)
   ((= value 3) 'glk-filemode-readwrite)
   ((= value 5) 'glk-filemode-writeappend)))

(defun glx-32->glk-gestalt-selector (value)
  (cond
   ((= value 0) 'glk-gestalt-version)
   ((= value 15) 'glk-gestalt-unicode)))

(defun glx-32->glk-boolean (value)
  (not (glx-0-p value)))

(defun glx-glk-opq->glx-32 (value)
  (if (null value)
      glx-0
    (let ((opq-symbol (read (symbol-name value))))
      (if (symbolp opq-symbol)
          opq-symbol
        (apply #'glx-32 (nreverse opq-symbol))))))

(defun glx-glk-result->32 (result)
  (cond ((null result) glx-0)
        ((and (booleanp result) result) glx-1)
        ((glki-opq-window-p result) (glx-glk-opq->glx-32 (glki-opq-window-glk-window-id result)))
        ((glki-opq-stream-p result) (glx-glk-opq->glx-32 (glki-opq-stream-glk-stream-id result)))
        ((glki-opq-fileref-p result) (glx-glk-opq->glx-32 (glki-opq-fileref-glk-fileref-id result)))
        ((symbolp result) result)
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
                             (glx-32->glk-window glx-0)
                             0
                             0) event-memptr))

(defun glx-glk-event-callback (event)
  (setq *glx-unexpected-exit* t)
  (glx-glk-store-event event *glx-store-event-memptr*)
  (unwind-protect
      (progn
        (while (glx-execute-next-instruction)))
    (glx-cleanup)))

(defun glx-store-glk-result (result memptr)
  (glx-store-glk-structure memptr (list (glx-32 result))))

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
   ((eq event-type 'glk-evtype-charinput) 2)
   ((eq event-type 'glk-evtype-lineinput) 3)))

(defun glx-glk-store-event (event memptr)
  (glx-log "storing event to location: %S - %S" memptr event)
  (glx-store-glk-structure memptr
                           (list (glx-32 (glx-glk-event-type->int (car event)))
                                 (if (cadr event) (glx-glk-opq->glx-32 (glki-opq-window-glk-window-id (cadr event))) glx-0)
                                 (glx-32 (nth 2 event))
                                 (glx-32 (nth 3 event))))
  (when (eq (car event) 'glk-evtype-lineinput)
    (glx-memory-set-string (nth 4 event) (nth 5 event))))

(defun glx-memory-set-unicode-string (memptr string)
  (mapcar (lambda (c) (glx-memory-set memptr (glx-32 c) 4) (setq memptr (glx-+ glx-4 memptr))) string))

(defun glx-glk-store-closed-stream (stream memptr)
  "Ignores STREAM unless it is a memory stream. Otherwise stores 
the read count and the write count and the data from STREAM."
  (when (car stream) 
    (glx-log "storing stream to location: %S - %S" memptr stream)
    (glx-store-glk-structure memptr
                             (list (glx-32 (cadr stream))
                                   (glx-32 (nth 2 stream))))
    (if (nth 3 stream)
        (glx-memory-set-unicode-string (nth 4 stream) (nth 5 stream))
      (glx-memory-set-string (nth 4 stream) (nth 5 stream)))))

(defun glx-glk-load-string-buffer (memptr buflen)
  (let (chars)
    (dotimes (i (glx-32->int buflen) (apply #'string (nreverse chars)))
      (push (glx-memory-get-byte-int memptr) chars)
      (setq memptr (glx-+1 memptr)))))

(defun glx-glk-load-unicode-string-buffer (memptr buflen)
  (let (chars)
    (dotimes (i (glx-32->int buflen) (apply #'string (nreverse chars)))
      (push (glx-32->unicode-char (glx-memory-get-32 memptr)) chars)
      (setq memptr (glx-+ glx-4 memptr)))))

(defun glx-glk-store-string (string memptr)
  (dotimes (n (length string))
    (glx-memory-set memptr (aref string n) 1)
    (setq memptr (glx-+1 memptr))))

(defun glx-glk-store-string-uni (string memptr)
  (dotimes (n (length string))
    (glx-memory-set memptr (glx-32 (aref string n)) 4)
    (setq memptr (glx-+ memptr glx-4))))

(defun glx-glk-store-unicode-string-buffer (str memptr buflen)
  (dotimes (i (glx-32->int buflen))
    (when (< i (length str))
      (glx-memory-set memptr (glx-32 (encode-char (aref str i) 'unicode)) 4)
      (setq memptr (glx-+ glx-4 memptr)))))

(defun glx-glk-load-string (memptr)
  (let (char
        result)
    (setq memptr (glx-+1 memptr))    
    (while (not (zerop (setq char (glx-memory-get-byte-int memptr))))
      (push char result)
      (setq memptr (glx-+1 memptr)))
    (apply #'string (nreverse result))))

(defun glx-glk-load-string-uni (memptr)
  (let (char
        result)
    (setq memptr (glx-+ glx-4 memptr))
    (while (not (glx-0-p (setq char (glx-memory-get-32 memptr))))
      (push (glx-32->unicode-char char) result)
      (setq memptr (glx-+ glx-4 memptr)))
    (apply #'string (nreverse result))))

(provide 'glx-glk)
