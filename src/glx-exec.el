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

(require 'glx-stack)

(defconst glx-instructions (make-hash-table))
(defconst glx-compiled-instructions (make-hash-table :test 'equal :size 100000))

(put 'glx-exec-error 'error-conditions '(error glx-error glx-exec-error))
(put 'glx-exec-error 'error-message "Glulx execution error")

(defun glx-get-opcode (memptr)
  "Returns the opcode at the location in the Glulx VM memory given by MEMPTR."
  (let ((opcode-type (lsh (glx-memory-get-byte-int memptr) -6)))
    ;; TODO this is not safe if you change how a 32 bit is represented.
    (cond ((= opcode-type 3) (let ((opcode (glx-32-get-bytes-as-list-big-endian (glx-memory-get-32 memptr))))
                               (setf (first opcode) (- (first opcode) #xc0))
                               (values (glx-+ memptr glx-4) (glx-32->int (apply #'glx-32 (reverse opcode))))))
          ((= opcode-type 2) (values (glx-+ memptr glx-2) (- (glx-memory-get-16-int memptr) #x8000)))
          (t (values (glx-+1 memptr) (glx-memory-get-byte-int memptr))))))

(defun glx-get-mode (addressing-modes-ptr low-bits)
  (let ((mode-byte (glx-memory-get-byte-int addressing-modes-ptr)))
    (if low-bits (logand mode-byte #xf) (lsh mode-byte -4))))

(defun glx-decode-load-arg (mode arg-ptr)
  (cond ((= mode 0) (values glx-0 0))
        ((= mode 1) (values (glx-memory-get-byte-signed arg-ptr) 1))
        ((= mode 2) (values (glx-memory-get-16-signed arg-ptr) 2))
        ((= mode 3) (values (glx-memory-get-32 arg-ptr) 4))
        ((= mode 5) (values (glx-memory-get-32 (glx-memory-get-byte arg-ptr)) 1))
        ((= mode 6) (values (glx-memory-get-32 (glx-memory-get-16 arg-ptr)) 2))
        ((= mode 7) (values (glx-memory-get-32 (glx-memory-get-32 arg-ptr)) 4))
        ((= mode 8) (values (glx-value-pop) 0))
        ((= mode 9) (values (glx-get-local-at-offset (glx-memory-get-byte arg-ptr)) 1))
        ((= mode 10) (values (glx-get-local-at-offset (glx-memory-get-16 arg-ptr)) 2))
        ((= mode 11) (values (glx-get-local-at-offset (glx-memory-get-32 arg-ptr)) 4))
        ((= mode 13) (values (glx-memory-get-32 (glx-+ *glx-ram-start* (glx-memory-get-byte arg-ptr))) 1))
        ((= mode 14) (values (glx-memory-get-32 (glx-+ *glx-ram-start* (glx-memory-get-16 arg-ptr))) 2))
        ((= mode 15) (values (glx-memory-get-32 (glx-+ *glx-ram-start* (glx-memory-get-32 arg-ptr))) 4))
        (t (signal 'glx-exec-error (list "Unsupported addressing mode" mode)))))

(defun glx-check-for-no-return (value)
  (if (eq value 'glk-no-return) glx-0 value))

(defun glx-store-throw (&rest ignore) nil)
(defun glx-store-mem (addr value &optional bytes) (if (not (glx-0-p addr)) (glx-memory-set addr (glx-check-for-no-return value) (if (not bytes) 4 bytes))) value)
(defun glx-store-stack (ignore value &rest ignore-rest) (glx-value-push (glx-check-for-no-return value)) value)
(defun glx-store-local (offset value &rest ignore) (glx-store-local-at-offset offset (glx-check-for-no-return value)) value)

(defun glx-decode-store-arg (mode arg-ptr)
  (cond ((= mode 0) (values (list #'glx-store-throw nil) 0))
        ((= mode 5) (values (list #'glx-store-mem (glx-memory-get-byte arg-ptr)) 1))
        ((= mode 6) (values (list #'glx-store-mem (glx-memory-get-16 arg-ptr)) 2))
        ((= mode 7) (values (list #'glx-store-mem (glx-memory-get-32 arg-ptr)) 4))
        ((= mode 8) (values (list #'glx-store-stack nil) 0))
        ((= mode 9) (values (list #'glx-store-local (glx-memory-get-byte arg-ptr)) 1))
        ((= mode 10) (values (list #'glx-store-local (glx-memory-get-16 arg-ptr)) 2))
        ((= mode 11) (values (list #'glx-store-local (glx-memory-get-32 arg-ptr)) 4))
        ((= mode 13) (values (list #'glx-store-mem (glx-+ *glx-ram-start* (glx-memory-get-byte arg-ptr))) 1))
        ((= mode 14) (values (list #'glx-store-mem (glx-+ *glx-ram-start* (glx-memory-get-16 arg-ptr))) 2))
        ((= mode 15) (values (list #'glx-store-mem (glx-+ *glx-ram-start* (glx-memory-get-32 arg-ptr))) 4))
        (t (signal 'glx-exec-error (list "Unsupported addressing mode" mode)))))

(defun glx-process-args (arg-spec addressing-modes-ptr load-arg-processor store-arg-processor)
  (let ((number-of-args (length arg-spec)))
    (let ((low-bits t)
          (arg-ptr (glx-+ addressing-modes-ptr (glx-int->32 (/ (if (oddp number-of-args) (+ 1 number-of-args) number-of-args) 2))))
          result)
      (dolist (spec arg-spec (values arg-ptr (nreverse result)))
        (let ((mode (glx-get-mode addressing-modes-ptr low-bits)))
          (multiple-value-bind (arg bytes-used)
              (if (eq spec 'load)
                  (funcall load-arg-processor mode arg-ptr)
                (funcall store-arg-processor mode arg-ptr))
            (push arg result)
            (setq arg-ptr (glx-+ arg-ptr (glx-32 bytes-used)))
            (setq low-bits (not low-bits))
            (when low-bits (setq addressing-modes-ptr (glx-+1 addressing-modes-ptr)))))))))

(defun glx-decode-args (arg-spec addressing-modes-ptr)
  (glx-process-args arg-spec addressing-modes-ptr #'glx-decode-load-arg #'glx-decode-store-arg))

(defun glx-get-opcode-args (opcode addressing-modes)
  "Returns a pointer to the next instruction and a list of arguments for the instruction
whose addressing modes are at the location in the Glulx VM memory given by ADDRESSING-MODES"
  (let ((instruction (gethash opcode glx-instructions)))
    (if instruction
        (glx-decode-args (second instruction) addressing-modes)
      (signal 'glx-exec-error (list "Unknown opcode" opcode)))))

(defsubst glx-process-instruction-result (result)
  (not (or (eq result 'glk-no-return)
           (eq result 'glx-quit))))

(defun glx-execute-instruction (opcode args)
  (glx-log "executing instruction: %s" (glx-format-exec-log (first (gethash opcode glx-instructions)) args))
  (glx-process-instruction-result (apply (third (gethash opcode glx-instructions)) args)))

(defun glx-format-exec-log (opcode args)
  (apply #'concat
         (format "%15s " opcode)
         (mapcar #'(lambda (arg) (format "%8s " (if (symbolp (car arg))
                                               (format "%8x-%8s" (cond ((eql (car arg)
                                                                             #'glx-store-throw)
                                                                        0)
                                                                       ((eql (car arg)
                                                                             #'glx-store-mem)
                                                                        1)
                                                                       ((eql (car arg)
                                                                             #'glx-store-local)
                                                                        2)
                                                                       (t 3))
                                                       (glx-32->hex-string (cadr arg)))
                                             (glx-32->hex-string arg))))
                 args)))

(defun glx-32->hex-string (value)
  (if value
      (let ((result
             (apply #'format
                    "%02x%02x%02x%02x"
                    (glx-32-get-bytes-as-list-big-endian value))))
        (while (and (> (length result) 1)
                    (string= (substring result 0 1) "0"))
          (setf result
                (substring result 1)))
        result)
    "0"))

(defun glx-execute-next-instruction ()
  (if (not (gethash *glx-pc* glx-compiled-instructions))
    (multiple-value-bind (next-instruction compiled-instruction)
        (glx-compile-instruction *glx-pc*)
      (glx-log "Compiled instruction at %08x %s" (glx-32->int  *glx-pc*) compiled-instruction)
      (if compiled-instruction
          (puthash *glx-pc* (list compiled-instruction next-instruction) glx-compiled-instructions)))
    (let ((compiled-instruction (gethash *glx-pc* glx-compiled-instructions)))
      (if compiled-instruction
          (progn
            (glx-log "Executing compiled instruction at %08x %s" (glx-32->int *glx-pc*) compiled-instruction)
            (setq *glx-pc* (second compiled-instruction))
            (glx-process-instruction-result (glx-execute-compiled-instruction (first compiled-instruction))))
        (multiple-value-bind (addressing-modes opcode)
            (glx-get-opcode *glx-pc*)
          (glx-log "Could not compile instruction at %08x" (glx-32->int *glx-pc*))
          (multiple-value-bind (next-instruction args)
              (glx-get-opcode-args opcode addressing-modes)
            (setq *glx-pc* next-instruction)
            (glx-execute-instruction opcode args)))))))

(provide 'glx-exec)
