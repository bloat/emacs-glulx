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

(require 'glx-stack)

(defconst glx-instructions (make-hash-table))

(defsubst glx-instruction-name (instruction)
  (car instruction))

(defsubst glx-instruction-arg-list (instruction)
  (cadr instruction))

(defsubst glx-instruction-function (instruction)
  (nth 2 instruction))

(put 'glx-exec-error 'error-conditions '(error glx-error glx-exec-error))
(put 'glx-exec-error 'error-message "Glulx execution error")

(defvar *glx-compile* nil)

(defun glx-get-opcode (memptr)
  "An opcode can encoded into 1, 2 or 4 bytes. Returns the location of the data following
the opcode at the location MEMPTR (depending on how long the opcode is), and the decoded opcode."
  (let ((opcode-type (lsh (glx-memory-get-byte-int memptr) -6)))
    ;; TODO this is not safe if you change how a 32 bit is represented.
    (cond ((= opcode-type 3) (let ((opcode (glx-32-get-bytes-as-list-big-endian (glx-memory-get-32 memptr))))
                               (setf (car opcode) (- (car opcode) #xc0))
                               (list (glx-+ memptr glx-4) (glx-32->int (apply #'glx-32 (reverse opcode))))))
          ((= opcode-type 2) (list (glx-+ memptr glx-2) (- (glx-memory-get-16-int memptr) #x8000)))
          (t (list (glx-+1 memptr) (glx-memory-get-byte-int memptr))))))

(defun glx-get-mode (addressing-modes-ptr low-bits)
  (let ((mode-byte (glx-memory-get-byte-int addressing-modes-ptr)))
    (if low-bits (logand mode-byte #xf) (lsh mode-byte -4))))

(defun glx-decode-load-arg (mode arg-ptr)
  (cond ((= mode 0) (list glx-0 0))
        ((= mode 1) (list (glx-memory-get-byte-signed arg-ptr) 1))
        ((= mode 2) (list (glx-memory-get-16-signed arg-ptr) 2))
        ((= mode 3) (list (glx-memory-get-32 arg-ptr) 4))
        ((= mode 5) (list (glx-memory-get-32 (glx-memory-get-byte arg-ptr)) 1))
        ((= mode 6) (list (glx-memory-get-32 (glx-memory-get-16 arg-ptr)) 2))
        ((= mode 7) (list (glx-memory-get-32 (glx-memory-get-32 arg-ptr)) 4))
        ((= mode 8) (list (glx-value-pop) 0))
        ((= mode 9) (list (glx-get-local-at-offset (glx-memory-get-byte arg-ptr)) 1))
        ((= mode 10) (list (glx-get-local-at-offset (glx-memory-get-16 arg-ptr)) 2))
        ((= mode 11) (list (glx-get-local-at-offset (glx-memory-get-32 arg-ptr)) 4))
        ((= mode 13) (list (glx-memory-get-32 (glx-+ *glx-ram-start* (glx-memory-get-byte arg-ptr))) 1))
        ((= mode 14) (list (glx-memory-get-32 (glx-+ *glx-ram-start* (glx-memory-get-16 arg-ptr))) 2))
        ((= mode 15) (list (glx-memory-get-32 (glx-+ *glx-ram-start* (glx-memory-get-32 arg-ptr))) 4))
        (t (signal 'glx-exec-error (list "Unsupported addressing mode" mode)))))

(defun glx-decode-store-arg (mode arg-ptr)
  (cond ((= mode 0) (list (list #'glx-store-throw nil) 0))
        ((= mode 5) (list (list #'glx-store-mem (glx-memory-get-byte arg-ptr)) 1))
        ((= mode 6) (list (list #'glx-store-mem (glx-memory-get-16 arg-ptr)) 2))
        ((= mode 7) (list (list #'glx-store-mem (glx-memory-get-32 arg-ptr)) 4))
        ((= mode 8) (list (list #'glx-store-stack nil) 0))
        ((= mode 9) (list (list #'glx-store-local (glx-memory-get-byte arg-ptr)) 1))
        ((= mode 10) (list (list #'glx-store-local (glx-memory-get-16 arg-ptr)) 2))
        ((= mode 11) (list (list #'glx-store-local (glx-memory-get-32 arg-ptr)) 4))
        ((= mode 13) (list (list #'glx-store-mem (glx-+ *glx-ram-start* (glx-memory-get-byte arg-ptr))) 1))
        ((= mode 14) (list (list #'glx-store-mem (glx-+ *glx-ram-start* (glx-memory-get-16 arg-ptr))) 2))
        ((= mode 15) (list (list #'glx-store-mem (glx-+ *glx-ram-start* (glx-memory-get-32 arg-ptr))) 4))
        (t (signal 'glx-exec-error (list "Unsupported addressing mode" mode)))))

(defun glx-process-args (arg-spec addressing-modes-ptr load-arg-processor store-arg-processor)
  "Returns 3 values, the pointer to the next instruction following this instructions arguments, 
a list of the loaded arguments, and a list of the addressing modes used to load those arguments.
This function mostly deals with advancing the pointer and loading the modes, the actual args are
loaded by the supplied LOAD-ARG-PROCESSOR and STORE-ARG-PROCESSOR functions."
  (let ((number-of-args (length arg-spec)))
    (let ((low-bits t)
          (arg-ptr (glx-+ addressing-modes-ptr (glx-32 (/ (if (cl-oddp number-of-args) (+ 1 number-of-args) number-of-args) 2))))
          args
          modes)
      (dolist (spec arg-spec (list arg-ptr (nreverse args) (nreverse modes)))
        (let ((mode (glx-get-mode addressing-modes-ptr low-bits)))
          (cl-multiple-value-bind (arg bytes-used)
              (if (eq spec 'load)
                  (funcall load-arg-processor mode arg-ptr)
                (funcall store-arg-processor mode arg-ptr))
            (push arg args)
            (push mode modes)
            (setq arg-ptr (glx-+ arg-ptr (glx-32 bytes-used)))
            (setq low-bits (not low-bits))
            (when low-bits (setq addressing-modes-ptr (glx-+1 addressing-modes-ptr)))))))))

(defun glx-decode-args (arg-spec addressing-modes-ptr)
  (glx-process-args arg-spec addressing-modes-ptr #'glx-decode-load-arg #'glx-decode-store-arg))

(defun glx-get-opcode-args (opcode addressing-modes-ptr)
  "Returns a pointer to the next instruction and a list of arguments for the instruction
whose addressing modes are at the location in the Glulx VM memory given by ADDRESSING-MODES-PTR.
Also returns the addressing modes themselves. "
  (let ((instruction (gethash opcode glx-instructions)))
    (if instruction
        (glx-decode-args (glx-instruction-arg-list instruction) addressing-modes-ptr)
      (signal 'glx-exec-error (list "Unknown opcode" opcode)))))

(defsubst glx-process-instruction-result (result)
  (not (or (eq result 'glk-no-return)
           (eq result 'glx-quit)
           (eq result 'glx-return-to-emacs))))

(defun glx-execute-instruction (opcode args modes)
  (let ((instruction (gethash opcode glx-instructions)))
    (glx-log "executing instruction: %s" (glx-format-exec-log (glx-instruction-name instruction) args))
    (glx-process-instruction-result (apply (glx-instruction-function instruction) modes args))))

(defun glx-format-exec-log (opcode args)
  (apply #'concat
         (format "%15s " opcode)
         (mapcar (lambda (arg) (format "%8s " (if (and (listp arg) (symbolp (car arg)))
                                                  (format "%8x-%8s"
                                                          (cond ((eql (car arg) #'glx-store-throw) 0)
                                                                ((eql (car arg) #'glx-store-mem) 1)
                                                                ((eql (car arg) #'glx-store-local) 2)
                                                                (t 3))
                                                          (glx-32->hex-string (cadr arg)))
                                                (glx-32->hex-string arg))))
                 args)))

(defun glx-32->hex-string (value)
  (if value
      (let ((result (apply #'format "%02x%02x%02x%02x" (glx-32-get-bytes-as-list-big-endian value))))
        (while (and (> (length result) 1) (string= (substring result 0 1) "0"))
          (setf result (substring result 1)))
        result)
    "0"))

(defun glx-execute-uncompiled-instruction ()
  (cl-multiple-value-bind (addressing-modes-ptr opcode) (glx-get-opcode *glx-pc*)
    (glx-log "Could not compile instruction at %08x" (glx-32->int *glx-pc*))
    (cl-multiple-value-bind (next-instruction args modes)
        (glx-get-opcode-args opcode addressing-modes-ptr)
      (setq *glx-pc* next-instruction)
      (glx-execute-instruction opcode args modes))))

(defun glx-execute-next-instruction ()
  (if *glx-compile*
      (progn
        (if (not (gethash *glx-pc* *glx-compiled-instructions*))
            (cl-multiple-value-bind (next-instruction compiled-instruction) (glx-compile-instruction *glx-pc*)
              (glx-log "Compiled instruction at %08x %s" (glx-32->int  *glx-pc*) compiled-instruction)
              (when compiled-instruction (puthash *glx-pc* (list compiled-instruction next-instruction) *glx-compiled-instructions*))))
        (let ((compiled-instruction (gethash *glx-pc* *glx-compiled-instructions*)))
          (if compiled-instruction
              (progn
                (glx-log "Executing compiled instruction at %08x %s" (glx-32->int *glx-pc*) compiled-instruction)
                (setq *glx-pc* (cadr compiled-instruction))
                (glx-process-instruction-result (glx-execute-compiled-instruction (car compiled-instruction))))
            (glx-execute-uncompiled-instruction))))
    (glx-execute-uncompiled-instruction)))

(defun glx-call-function-and-return-to-emacs (memptr args)
  "Calls the function at MEMPTR and keeps running the Glulx VM
calling more functions as required until this top-level function
returns."
  (glx-call-function memptr 'glx-return-to-emacs 0 args)
  (while (glx-execute-next-instruction)))

(put 'glx-compile-error 'error-conditions '(error glx-error glx-compile-error))
(put 'glx-compile-error 'error-message "Glulx compilatation error")

(defun glx-compile-function (function-ptr)
  ;; TODO
  )

;; TODO this function is not called yet
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
  "The first result is a list of a function to run to retrieve the actual value for the argument,
and a value to pass as an argument to that function when running it. The second result is the 
number of bytes used from the instructions argument data."
  (cond ((= mode 0) (list (list (lambda (loaded-arg-ptr) glx-0) nil) 0))
        ((= mode 1) (list (list #'identity (glx-memory-get-byte-signed arg-ptr)) 1))
        ((= mode 2) (list (list #'identity (glx-memory-get-16-signed arg-ptr)) 2))
        ((= mode 3) (list (list #'identity (glx-memory-get-32 arg-ptr)) 4))
        ((= mode 5) (list (list (lambda (loaded-arg-ptr) (glx-memory-get-32 loaded-arg-ptr)) (glx-memory-get-byte arg-ptr)) 1))
        ((= mode 6) (list (list (lambda (loaded-arg-ptr) (glx-memory-get-32 loaded-arg-ptr)) (glx-memory-get-16 arg-ptr)) 2))
        ((= mode 7) (list (list (lambda (loaded-arg-ptr) (glx-memory-get-32 loaded-arg-ptr)) (glx-memory-get-32 arg-ptr)) 4))
        ((= mode 8) (list (list (lambda (loaded-arg-ptr) (glx-value-pop)) nil) 0))
        ((= mode 9) (list (list (lambda (loaded-arg-ptr) (glx-get-local-at-offset loaded-arg-ptr)) (glx-memory-get-byte arg-ptr)) 1))
        ((= mode 10) (list (list (lambda (loaded-arg-ptr) (glx-get-local-at-offset loaded-arg-ptr)) (glx-memory-get-16 arg-ptr)) 2))
        ((= mode 11) (list (list (lambda (loaded-arg-ptr) (glx-get-local-at-offset loaded-arg-ptr)) (glx-memory-get-32 arg-ptr)) 4))
        ((= mode 13) (list (list (lambda (loaded-arg-ptr) (glx-memory-get-32 loaded-arg-ptr)) (glx-+ *glx-ram-start* (glx-memory-get-byte arg-ptr))) 1))
        ((= mode 14) (list (list (lambda (loaded-arg-ptr) (glx-memory-get-32 loaded-arg-ptr)) (glx-+ *glx-ram-start* (glx-memory-get-16 arg-ptr))) 2))
        ((= mode 15) (list (list (lambda (loaded-arg-ptr) (glx-memory-get-32 loaded-arg-ptr)) (glx-+ *glx-ram-start* (glx-memory-get-32 arg-ptr))) 4))
        (t (signal 'glx-exec-error (list "Unsupported addressing mode" mode)))))

(defun glx-compile-store-arg (mode arg-ptr)
  (cl-multiple-value-bind (store bytes-read)
      (glx-decode-store-arg mode arg-ptr)
    (list (list #'identity store) bytes-read)))

(defun glx-compile-args (arg-spec addressing-modes-ptr)
  (glx-process-args arg-spec addressing-modes-ptr #'glx-compile-load-arg #'glx-compile-store-arg))

(defun glx-compile-instruction (ptr)
  (cl-multiple-value-bind (addressing-modes-ptr opcode)
      (glx-get-opcode ptr)
    (let ((instruction (gethash opcode glx-instructions)))
      (if instruction
          (cl-multiple-value-bind (next-instruction compiled-args modes)
              (glx-compile-args (glx-instruction-arg-list instruction) addressing-modes-ptr)
            (list next-instruction (list (glx-instruction-function instruction) modes compiled-args)))
        (signal 'glx-exec-error (list "Unknown opcode" opcode))))))

(defun glx-execute-compiled-instruction (compiled-inst)
  (apply (car compiled-inst) ; The instruction function
         (cadr compiled-inst) ; The addressing modes used to load the args
         (mapcar #'(lambda (compiled-arg) (funcall (car compiled-arg) (cadr compiled-arg)))
                 (nth 2 compiled-inst)))) ; Call the functions to get the values for the instruction's arguments.

(provide 'glx-exec)
