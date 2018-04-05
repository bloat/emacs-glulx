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

(put 'glx-string-error 'error-conditions '(error glx-error glx-string-error))
(put 'glx-string-error 'error-message "Glulx VM string decoding error")

(defun glx-look-for-string-terminator (memptr)
  "MEMPTR is a 32 bit pointer to a Glulx VM memory location. Returns the location of the next
string terminator (a zero byte)."
  (if (and (= 0 (glx-memory-get-byte-int memptr)))
      memptr
    (glx-look-for-string-terminator (glx-+1 memptr))))

(defun glx-uncompressed-string-p (memptr)
  "Is the object at MEMPTR an uncompressed string?"
  (= #xe0 (glx-memory-get-byte-int memptr)))

(defun glx-compressed-string-p (memptr)
  "Is the object at MEMPTR a compressed string?"
  (= #xe1 (glx-memory-get-byte-int memptr)))

(defun glx-uncompressed-unicode-string-p (memptr)
  "Is the object at MEMPTR and uncompressed unicode string?"
  (= #xe2 (glx-memory-get-byte-int memptr)))

(defun glx-memory-get-unicode-string (memptr)
  "Loads a string consisting of unicode characters (four bytes each) from the 
Glulx VM memory location given by the 32 bit MEMPTR. The string is terminated
by a 32 bit 0."
  (let (char)
    (while (not (glx-0-p (setq char (glx-memory-get-32 memptr))))
      (funcall (glx-iosys-charfun) (glx-32->unicode-char char))
      (setq memptr (glx-+ glx-4 memptr)))))

(defun glx-memory-get-string (memptr)
  "Loads a string consisting of single byte characters from the 
Glulx VM memory location given by the 32 bit MEMPTR. The string is terminated
by 0 byte. The characters are outputted using the current GLX-IOSYS-CHARFUN."
  (let (char)
    (while (not (zerop (setq char (glx-memory-get-byte-int memptr))))
      (funcall (glx-iosys-charfun) char)
      (setq memptr (glx-+1 memptr)))))

(defun glx-get-string (memptr function-caller)
  "Process the string encoded at the Glulx VM memory location given by the 32 bit MEMPTR.
Individual characters from the string are passed to CHARFUN as they are loaded."
  (cond ((glx-uncompressed-string-p memptr)
         (glx-memory-get-string (glx-+1 memptr)))
        ((glx-compressed-string-p memptr)
         (glx-uncompress-string (glx-get-bitstream (glx-+1 memptr)) function-caller))
        ((glx-uncompressed-unicode-string-p memptr)
         (glx-memory-get-unicode-string (glx-+ glx-4 memptr)))
        (t (signal 'glx-string-error (list "Unknown string type" memptr (glx-memory-get-byte-int memptr))))))

(defun glx-st-get-root-node-ptr ()
  "Return a pointer to the root node of the currently selected string table."
  (glx-memory-get-32 (glx-+ glx-8 *glx-string-table*)))

(defun glx-get-bitstream (memptr)
  "Returns a bitstream starting with the least significant bit
at the Glulx VM memory location given by the 32 bit MEMPTR."
  (cons memptr 0))

(defun glx-next-bit (bitstream)
  "Returns the next bit in the given BITSTREAM"
  (let ((bit (not
              (= 0 (logand (glx-memory-get-byte-int (car bitstream))
                           (expt 2 (cdr bitstream)))))))
    (setf (cdr bitstream) (+ 1 (cdr bitstream)))
    (when (= 8 (cdr bitstream))
      (setf (car bitstream) (glx-+1 (car bitstream)))
      (setf (cdr bitstream) 0))
    bit))

(defun glx-string-or-function (memptr args function-caller)
  (let ((obj-type (glx-memory-get-byte-int memptr)))
    (cond ((or (= #xe0 obj-type) (= #xe1 obj-type) (= #xe2 obj-type)) (glx-get-string memptr function-caller))
          ((or (= #xc0 obj-type) (= #xc1 obj-type)) (funcall function-caller memptr args))
          (t (signal 'glx-string-error (list "Unknown object type" obj-type memptr))))))

(defun glx-string-node-args (memptr)
  (let (args)
    (dotimes (n (glx-32->int (glx-memory-get-32 memptr)) (nreverse args))
      (setq memptr (glx-+ glx-4 memptr))
      (push (glx-memory-get-32 memptr) args))))

(defun glx-uncompress-string (bitstream function-caller)
  "Uncompress a string by decoding the given BITSTREAM. Uses the
string table at *GLX-STRING-TABLE*. Characters from the uncompressed 
string are passed to the current GLX-IOSYS-CHARFUN. If there are any
glulx functions to call while uncompressing, they are handled with the 
FUNCTION-CALLER."
  (let ((node (glx-st-get-root-node-ptr)))
    (let ((node-type (glx-memory-get-byte-int node)))
      (while (not (= node-type 1))
        (cond ((= 0 node-type) (setq node (glx-memory-get-32 (glx-+ (if (glx-next-bit bitstream) glx-5 glx-1) node))))
              ((= 2 node-type) (funcall (glx-iosys-charfun) (glx-memory-get-byte-int (glx-+1 node)))
               (setq node (glx-st-get-root-node-ptr)))
              ((= 3 node-type) (glx-memory-get-string (glx-+1 node))
               (setq node (glx-st-get-root-node-ptr)))
              ((= 4 node-type) (funcall (glx-iosys-charfun) (glx-32->unicode-char (glx-memory-get-32 (glx-+1 node))))
               (setq node (glx-st-get-root-node-ptr)))
              ((= 5 node-type) (glx-memory-get-unicode-string (glx-+1 node))
               (setq node (glx-st-get-root-node-ptr)))
              ((= 8 node-type) (glx-string-or-function (glx-memory-get-32 (glx-+1 node)) nil function-caller)
               (setq node (glx-st-get-root-node-ptr)))
              ((= 9 node-type) (glx-string-or-function (glx-memory-get-32 (glx-memory-get-32 (glx-+1 node))) nil function-caller)
               (setq node (glx-st-get-root-node-ptr)))
              ((= 10 node-type) (glx-string-or-function (glx-memory-get-32 (glx-+1 node)) (glx-string-node-args (glx-+ node glx-5)) function-caller)
               (setq node (glx-st-get-root-node-ptr)))
              ((= 11 node-type) (glx-string-or-function (glx-memory-get-32 (glx-memory-get-32 (glx-+1 node))) (glx-string-node-args (glx-+ node glx-5)) function-caller)
               (setq node (glx-st-get-root-node-ptr)))
              (t (signal 'glx-string-error (list "Unknown node type" node-type node))))
        (setq node-type (glx-memory-get-byte-int node))))))

(provide 'glx-string)
