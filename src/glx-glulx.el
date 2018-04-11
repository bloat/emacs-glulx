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

(defvar *glx-memory* nil "The Glulx VM memory vector")
(defvar *glx-original-memory* nil "A copy of the original memory state, for restart")
(defvar *glx-stack* nil "The Glulx VM stack")
(defvar *glx-pc* nil "The Glulx VM program counter")
(defvar *glx-string-table* nil "The Glulx VM string table")
(defvar *glx-ram-start* nil "A pointer to the location in memory of the start of RAM")
(defvar *glx-iosys* (list (lambda (c)) glx-0 glx-0) "The current io system")
(defvar *glx-undo* nil "Undo information for the Glulx VM")
(defvar *glx-glk-selected* nil "True if we are using the glk output system")
(defvar *glx-glk-id-gen* 0 "Generates glk ids for new glk opaque objects")
(defvar *glx-store-event-memptr* nil "Where to store the glk event on re-entry")
(defvar *glx-catch-token* glx-0 "An increasing number for catch tokens")
(defvar *glx-protect* nil "The memory protection range")
(defvar *glx-log-buffer* nil "A buffer for Glulx VM logging output")
(defvar *glx-accelerated-parameters* nil "An array of parameters for accelerated functions")
(defvar *glx-accelerated-functions* nil "A hashtable to store which functions have been accelerated")
(defvar *glx-compiled-instructions* nil "A hashtable to store all the instructions which have been compiled")

(defsubst glx-memory-ref (memptr-int)
  (aref *glx-memory* memptr-int))

(defsubst glx-memory-get-byte-int (memptr)
  "Returns an integer with the value of the byte at the Glulx VM memory
location given by the 32 bit MEMPTR."
  (glx-memory-ref (glx-32->int memptr)))

(defun glx-memory-get-byte (memptr)
  "Returns a glx-32 with the value of the byte at the Glulx VM memory
location given by the 32 bit MEMPTR."
  (glx-32 (glx-memory-get-byte-int memptr)))

(defun glx-sign-extend-byte-to-32 (value)
  (let ((unsigned (glx-32->int value)))
    (if (> unsigned #x7f)
        (glx-32 unsigned 255 255 255)
      value)))

(defun glx-sign-extend-16-to-32 (value)
  (let ((unsigned (glx-32->int value))
        (bytes (glx-32-get-bytes-as-list-big-endian value)))
    (if (> unsigned #x7fff)
        (glx-32 (nth 3 bytes) (nth 2 bytes) 255 255)
      value)))

(defun glx-memory-get-byte-signed (memptr)
  "Returns a 32 bit value with the value of the byte at the Glulx VM memory
location given by the 32 bit MEMPTR. The value is sign extended from 8 to 32 bits."
  (glx-sign-extend-byte-to-32 (glx-memory-get-byte memptr)))

(defun glx-memory-get-range-as-vector (start end)
  "Return a vector of bytes from the Glulx VM memory locations given by
START (inclusive) and END (exclusive)."
  (cl-subseq *glx-memory* (glx-32->int start) (glx-32->int end)))

(defun glx-memory-get-range (start end)
  "Return a list of bytes from the Glulx VM memory locations given by
START (inclusive) and END (exclusive)."
  (append (glx-memory-get-range-as-vector start end) nil))

(defun glx-memory-get-value (memptr length)
  (apply #'glx-32 (nreverse (glx-memory-get-range memptr (glx-+ length memptr)))))

(defun glx-memory-get-16 (memptr)
  "Returns a 32 bit value with the value of the two bytes at the Glulx VM memory
location given by the 32 bit MEMPTR."
  (glx-memory-get-value memptr glx-2))

(defun glx-memory-get-16-signed (memptr)
  "Returns a 32 bit value with the value of the two bytes at the Glulx VM memory
location given by the 32 bit MEMPTR. The value is sign extended from 16 to 32 bits."
  (glx-sign-extend-16-to-32 (glx-memory-get-16 memptr)))

(defun glx-memory-get-16-int (memptr)
  "Returns an integer with the value of the two bytes at the Glulx VM memory
location given by the 32 bit MEMPTR."
  (glx-32->int (glx-memory-get-value memptr glx-2)))

(defun glx-memory-get-32 (memptr)
  "Returns a 32 bit value from the location given by the 32 bit MEMPTR"
  (glx-memory-get-value memptr glx-4))

(defun glx-memory-set (memptr value bytes)
  "Sets a value into the memory location given by the 32 bit MEMPTR.
The value is truncated to the given number of bytes."
  (let ((ptr (glx-32->int memptr)))
    (dolist (byte (cl-subseq (glx-32-get-bytes-as-list-big-endian value) (- bytes)))
      (aset *glx-memory* ptr byte)
      (cl-incf ptr))))

(defun glx-memory-set-string (memptr string)
  (let ((ptr (glx-32->int memptr)))
    (mapcar (lambda (char) (aset *glx-memory* ptr char)
              (cl-incf ptr))
            string)))

(defun glx-log (message &rest values)
  (unless *glx-log-buffer*
    (setq *glx-log-buffer* (get-buffer-create "*glx-log*")))
  (with-current-buffer *glx-log-buffer*
    (insert (apply #'format message values) ?\n)))

;; (defsubst glx-log (message &rest values))

(defun glx-search-get-key (key key-size options)
  (if (= 0 (logand 1 (glx-32->int options)))
      (cl-subseq (glx-32-get-bytes-as-list-big-endian key) (- (glx-32->int key-size)))
    (glx-search-load-key key 0 glx-0 glx-0 key-size)))

(defun glx-search-struct-start (start count struct-size)
  (glx-+ start (glx-* (glx-32 count) struct-size)))

(defun glx-search-load-key (start count struct-size key-offset key-size)
  (let ((key-start (glx-+ key-offset (glx-search-struct-start start count struct-size))))
    (glx-memory-get-range key-start (glx-+ key-start key-size))))

(defun glx-search-success (start count struct-size options)
  (if (= 0 (logand 4 (glx-32->int options)))
      (glx-search-struct-start start count struct-size)
    (glx-32 count)))

(defun glx-search-failure (options)
  (if (= 0 (logand 4 (glx-32->int options)))
      glx-0
    (glx-32 -1)))

(defun glx-memory-linear-search (key key-size start struct-size num-structs key-offset options)
  (let ((actual-key (glx-search-get-key key key-size options))
        (count 0)
        (result nil)
        (int-num-structs (if (equal num-structs (glx-32 -1)) -1 (glx-32->int num-structs)))
        (zero-term-flag (= 2 (logand (glx-32->int options) 2))))
    (while (not result)
      (if (= count int-num-structs)
          (setq result (glx-search-failure options))
        (let ((loaded-key (glx-search-load-key start count struct-size key-offset key-size)))
          (cond ((and zero-term-flag (not (cl-remove-if #'zerop loaded-key)) (cl-remove-if #'zerop actual-key))
                 (setq result (glx-search-failure options)))
                ((equal actual-key loaded-key)
                 (setq result (glx-search-success start count struct-size options))))))
      (cl-incf count))
    result))

(defun glx-binary-key-compare (key1 key2)
  (cond ((null key1) 0)
        ((< (car key1) (car key2)) -1)
        ((> (car key1) (car key2)) 1)
        (t (glx-binary-key-compare (cdr key1) (cdr key2)))))

(defun glx-memory-binary-search (key key-size start struct-size num-structs key-offset options)
  (let ((actual-key (glx-search-get-key key key-size options))
        (left -1)
        (right (glx-32->int num-structs))
        (count 0)
        (result nil))
    (while (not result)
      (let ((probe (/ (- right left) 2)))
        (if (= probe 0)
            (setq result (glx-search-failure options))
          (setq count (+ left probe))
          (let* ((loaded-key (glx-search-load-key start count struct-size key-offset key-size))
                 (comp (glx-binary-key-compare loaded-key actual-key)))
            (cond ((= 0 comp)
                   (setq result (glx-search-success start count struct-size options)))
                  ((= -1 comp)
                   (setq left count))
                  (t (setq right count)))))))
    result))

(defun glx-memory-linked-search (key key-size start key-offset next-offset options)
  (let ((actual-key (glx-search-get-key key key-size options))
        (result nil)
        (zero-term-flag (= 2 (logand (glx-32->int options) 2)))
        (end-of-list nil))
    (while (not result)
      (if end-of-list
          (setq result glx-0)
        (let ((loaded-key (glx-search-load-key start 0 glx-0 key-offset key-size)))
          (cond ((and zero-term-flag (not (cl-remove-if #'zerop loaded-key)) (cl-remove-if #'zerop actual-key))
                 (setq result (glx-search-failure options)))
                ((equal actual-key loaded-key)
                 (setq result start)))))
      (setq start (glx-memory-get-32 (glx-+ start next-offset)))
      (setq end-of-list (glx-0-p start)))
    result))

(defun glx-memory-mzero (count memptr)
  "Write COUNT zero bytes to memory starting at the address given by MEMPTR"
  (unless (glx-0-p count)
    (let ((ptr (glx-32->int memptr)))
      (dotimes (n (glx-32->int count))
        (aset *glx-memory* (+ ptr n) 0)))))

(defun glx-memory-mcopy (count source target)
  (let ((sourcei (glx-32->int source))
        (targeti (glx-32->int target))
        (counti (glx-32->int count)))
    (if (< targeti sourcei)
        (dotimes (n counti)
          (aset *glx-memory* (+ targeti n) (aref *glx-memory* (+ sourcei n))))
      (dotimes (n counti)
        (aset *glx-memory* (+ targeti (- (- counti 1) n)) (aref *glx-memory* (+ sourcei (- (- counti 1) n))))))))

(defun glx-save-undo (store)
  (setq *glx-undo*
        (list (copy-sequence *glx-memory*)
              (copy-tree *glx-stack*)
              *glx-pc*
              store)))

(defun glx-restore-memory-with-protect (incoming)
  (if *glx-protect*
      (dotimes (n (cdr *glx-protect*) incoming)
        (let ((memptr (+ n (car *glx-protect*))))
          (aset incoming
                memptr
                (if (< memptr (length *glx-memory*))
                    (glx-memory-ref memptr)
                  0))))
    incoming))

(defun glx-restore-undo ()
  (when *glx-undo*
    (setq *glx-memory* (glx-restore-memory-with-protect (car *glx-undo*)))
    (setq *glx-stack* (cadr *glx-undo*))
    (setq *glx-pc* (nth 2 *glx-undo*))
    (let ((undo-store (nth 3 *glx-undo*)))
      (funcall (car undo-store) (cadr undo-store) (glx-32 -1) 4))
    (setq *glx-undo* nil)
    t))

(defun glx-gen-inst-function (name)
  (intern (concat "glx-instruction-" (symbol-name name))))

(defun glx-iosys-charfun ()
  (if *glx-iosys*
      (car *glx-iosys*)
    (signal 'glx-glk-error (list "No iosys selected"))))

(defun glx-iosys-rock ()
  (if *glx-iosys*
      (cadr *glx-iosys*)
    (signal 'glx-glk-error (list "No iosys selected"))))

(defun glx-iosys-id ()
  (if *glx-iosys*
      (nth 2 *glx-iosys*)
    (signal 'glx-glk-error (list "No iosys selected"))))

(defun glx-set-memory-size (new-size)
  (let ((current-size (length *glx-memory*)))
    (if (>= new-size current-size)
        (setq *glx-memory* (vconcat *glx-memory* (make-vector (- new-size current-size) 0)))
      (unless (< new-size (glx-32->int (glx-memory-get-32 (glx-32 16))))
        (let ((new-memory (make-vector new-size 0)))
          (dotimes (n new-size)
            (aset new-memory n (aref *glx-memory* n)))
          (setq *glx-memory* new-memory))))))

(defun expand-memory-vector (memory)
  "Expand the memory vector based on the values of EXTMEM"
  (vconcat memory
           (make-vector
            (glx-32->int
             (glx-- (glx-32 (aref memory 19) (aref memory 18) (aref memory 17) (aref memory 16))
                    (glx-32 (aref memory 15) (aref memory 14) (aref memory 13) (aref memory 12))))
            0)))

(provide 'glx-glulx)
