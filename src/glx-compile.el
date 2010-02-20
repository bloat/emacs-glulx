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

(require 'glx-exec)

(put 'glx-compile-error 'error-conditions '(error glx-error glx-compile-error))
(put 'glx-compile-error 'error-message "Glulx compilatation error")

(defun glx-compile-rom (function-ptr)
  "Compiles the function passed in. Maintains a list of new functions found
during compilation, and compiles those also."
  (let ((functions-to-compile (list function-ptr))
        (compiled-functions nil))
    (while functions-to-compile
      (let ((to-compile (pop functions-to-compile)))
        (unless (memq to-compile compiled-functions)
          (setf functions-to-compile
                (append functions-to-compile
                        (glx-compile-function to-compile)))
          (push to-compile compiled-functions))))))

(defun glx-compile-load-arg (mode arg-ptr)
  (cond ((= mode 0) (values (list #'(lambda (loaded-arg-ptr) glx-0) nil) 0))
        ((= mode 1) (values (list #'identity (glx-memory-get-byte-signed arg-ptr)) 1))
        ((= mode 2) (values (list #'identity (glx-memory-get-16-signed arg-ptr)) 2))
        ((= mode 3) (values (list #'identity (glx-memory-get-32 arg-ptr)) 4))
        ((= mode 5) (values (list #'(lambda (loaded-arg-ptr) (glx-memory-get-32 loaded-arg-ptr)) (glx-memory-get-byte arg-ptr)) 1))
        ((= mode 6) (values (list #'(lambda (loaded-arg-ptr) (glx-memory-get-32 loaded-arg-ptr)) (glx-memory-get-16 arg-ptr)) 2))
        ((= mode 7) (values (list #'(lambda (loaded-arg-ptr) (glx-memory-get-32 loaded-arg-ptr)) (glx-memory-get-32 arg-ptr)) 4))
        ((= mode 8) (values (list #'(lambda (loaded-arg-ptr) (glx-value-pop)) nil) 0))
        ((= mode 9) (values (list #'(lambda (loaded-arg-ptr) (glx-get-local-at-offset loaded-arg-ptr)) (glx-memory-get-byte arg-ptr)) 1))
        ((= mode 10) (values (list #'(lambda (loaded-arg-ptr) (glx-get-local-at-offset loaded-arg-ptr)) (glx-memory-get-16 arg-ptr)) 2))
        ((= mode 11) (values (list #'(lambda (loaded-arg-ptr) (glx-get-local-at-offset loaded-arg-ptr)) (glx-memory-get-32 arg-ptr)) 4))
        ((= mode 13) (values (list #'(lambda (loaded-arg-ptr) (glx-memory-get-32 loaded-arg-ptr)) (glx-+ *glx-ram-start* (glx-memory-get-byte arg-ptr))) 1))
        ((= mode 14) (values (list #'(lambda (loaded-arg-ptr) (glx-memory-get-32 loaded-arg-ptr)) (glx-+ *glx-ram-start* (glx-memory-get-16 arg-ptr))) 2))
        ((= mode 15) (values (list #'(lambda (loaded-arg-ptr) (glx-memory-get-32 loaded-arg-ptr)) (glx-+ *glx-ram-start* (glx-memory-get-32 arg-ptr))) 4))
        (t (signal 'glx-exec-error (list "Unsupported addressing mode" mode)))))

(defun glx-compile-store-arg (mode arg-ptr)
  (multiple-value-bind (store bytes-read)
      (glx-decode-store-arg mode arg-ptr)
    (values (list #'identity store) bytes-read)))

(defun glx-compile-args (arg-spec addressing-modes)
  (glx-process-args arg-spec addressing-modes #'glx-compile-load-arg #'glx-compile-store-arg))

(defun glx-compile-instruction (ptr)
  (multiple-value-bind (addressing-modes opcode)
      (glx-get-opcode ptr)
    (let ((instruction (gethash opcode glx-instructions)))
      (if instruction
          (multiple-value-bind (next-instruction compiled-args)
              (glx-compile-args (second instruction) addressing-modes)
            (values next-instruction (list (third instruction) compiled-args)))
        (signal 'glx-exec-error (list "Unknown opcode" opcode))))))

(defun glx-execute-compiled-instruction (compiled-inst)
  (apply (first compiled-inst)
         (mapcar #'(lambda (compiled-arg) (funcall (first compiled-arg) (second compiled-arg)))
                 (second compiled-inst))))

(provide 'glx-compile)
