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

(require 'glx-glulx)

(put 'glx-stack-error 'error-conditions '(error glx-error glx-stack-error))
(put 'glx-stack-error 'error-message "Glulx VM stack error")

(defun glx-stack-push (value)
  (push value *glx-stack*))

(defun glx-stack-pop ()
  (pop *glx-stack*))

(defun glx-value-push (value)
  (push value (caar *glx-stack*)))

(defun glx-value-pop ()
  (if (not (caar *glx-stack*))
      (signal 'glx-stack-error (list "Can't pop beyond frame length" (car *glx-stack*)))
    (pop (caar *glx-stack*))))

(defun glx-stack-peek (count)
  (if (> count (length (caar *glx-stack*)))
      (signal 'glx-stack-error (list "Can't peek beyond frame length" (car *glx-stack*)))
    (subseq (caar *glx-stack*) 0 count)))

(defun glx-push-call-stub (dest-type dest-addr)
  (glx-stack-push (list dest-type dest-addr *glx-pc*)))

(defun glx-pad-2-p (length alignment)
  "Should we pad for a two byte alignment?"
  (and (oddp length) (= alignment 2)))

(defun glx-pad-4-p (length alignment)
  "Should we pad for a four byte alignment?"
  (and (not (= 0 (% length 4))) (= alignment 4)))

(defun glx-get-empty-locals-list (format)
  (let ((format-length (length format))
        (ptr 1)
        (result '())
        (addr 0))
    (while (< ptr format-length)
      (let ((var-size (aref format (- ptr 1))))
        (dotimes (count (aref format ptr))
          (push (cons (glx-32 addr) glx-0) result)
          (incf addr var-size)))
      (unless (= format-length (+ 1 ptr))
        (let ((next-size (aref format (+ 1 ptr))))
          (incf addr
                (cond ((glx-pad-4-p addr next-size) (- 4 (% addr 4)))
                      ((glx-pad-2-p addr next-size) 1)
                      (t 0)))))
      (incf ptr 2))
    (nreverse result)))

(defun glx-get-local-at-offset (offset)
  (let ((result (assoc offset (cadar *glx-stack*))))
    (if result (cdr result) (signal 'glx-stack-error (list "Unknown local" offset (car *glx-stack*))))))

(defun glx-store-local-at-offset (offset value)
  (let ((result (assoc offset (cadar *glx-stack*))))
    (if result (setf (cdr result) value)
      (signal 'glx-stack-error (list "Unknown local" offset (car *glx-stack*))))))

(defun glx-look-for-format-of-locals-terminator (memptr)
  (while (not (glx-0-p (glx-memory-get-16 memptr)))
    (setf memptr (glx-+1 memptr)))
  memptr)

(defun glx-get-format-of-locals-from-function-def (function-ptr)
  "FUNCTION-PTR is a 32 bit pointer to a Glulx VM memory location, assumed to
be the start of a function. Returns the format of locals specifier."
  (let ((locals (glx-+1 function-ptr)))
    (glx-memory-get-range-as-vector locals (glx-look-for-format-of-locals-terminator locals))))

(defun glx-build-call-frame (function-ptr)
  "Build a Glulx VM call frame on the stack, for calling a function at the given 32 bit FUNCTION-PTR."
  (let* ((format-of-locals (glx-get-format-of-locals-from-function-def function-ptr))
         (locals (glx-get-empty-locals-list format-of-locals)))
    (glx-stack-push (list nil locals))))

(defun glx-get-function-code-start (function-ptr)
  "Finds the first opcode in the given function"
  (glx-+ (glx-look-for-format-of-locals-terminator function-ptr) glx-2))

(defun glx-handle-function-args (function-ptr args)
  "Puts the arguments to a function on the stack in the right place. Either in
the locals section of the call frame, or pushed onto the stack following the call frame."

  ;; Handle a stack style function...
  (if (= #xc0 (glx-memory-get-byte-int function-ptr))
      (progn
        (dolist (arg (reverse args))
          (glx-value-push arg))
        (glx-value-push (glx-32 (length args))))

    ;; Or else a call frame style function
    (let ((format-pos (glx-+1 function-ptr))
          (call-frame (glx-stack-pop))
          (local-pos 0))

      ;; Do we have any more args? And are we at the end of the format?
      (while (and (car args) (not (glx-0-p (glx-memory-get-16 format-pos))))

        ;; Copy all variables for this section of the format
        (let ((local-length (glx-memory-get-byte-int format-pos)))
          (dotimes (local-count (glx-memory-get-byte-int (glx-+1 format-pos)))
            (setf (cdr (nth local-pos (cadr call-frame))) (glx-truncate (car args) local-length))
            (incf local-pos)
            (setq args (cdr args))))

        ;; Move to the next section of the format
        (setq format-pos (glx-+ format-pos glx-2)))

      (glx-stack-push call-frame))))

(defun glx-call-function (function-ptr dest-type dest-addr args)
  "Calls the function pointed to by FUNCTION-PTR. Sets the PC and the Frame Pointer
appropriately. Pushes necessary data structures onto the stack. The call stub will
contain DEST-TYPE and DEST-ADDR."
  (glx-push-call-stub dest-type dest-addr)
  (glx-build-call-frame function-ptr)
  (glx-handle-function-args function-ptr args)
  (setq *glx-pc* (glx-get-function-code-start function-ptr)))

(defun glx-return-from-function ()
  "Removes the current call frame from the stack. Also removes and returns the
current call stub. Sets the PC back to the value stored in the call stub."
  (glx-stack-pop)
  (let ((call-stub (glx-stack-pop)))
    (setq *glx-pc* (third call-stub))
    call-stub))

(provide 'glx-stack)
