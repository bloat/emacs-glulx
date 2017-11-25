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

(defun glx-stack-count ()
  "The number of stack values in the current call frame"
  (let ((result 0))
    (dolist (item (caar *glx-stack*) (glx-32 result))
      (unless (eq 'catch (first item)) (incf result)))))

(defun glx-value-push (value)
  "Push a value into the stack of the current call frame"
  (push value (caar *glx-stack*)))

(defun glx-value-pop ()
  "Pop a value from the stack of the current call frame"
  (while (eq 'catch (caaaar *glx-stack*))
    (pop (caar *glx-stack*)))
  (if (not (caar *glx-stack*))
      (signal 'glx-stack-error (list "Can't pop beyond frame length" (car *glx-stack*)))
    (pop (caar *glx-stack*))))

(defun glx-stack-peek (count)
  "Peek at values on the stack (within the current call frame)"
  (let ((stack (caar *glx-stack*))
        result)
    (while (> count 0)
      (unless stack
        (signal 'glx-stack-error (list "Can't peek beyond frame length" (car *glx-stack*))))
      (unless (eq 'catch (caar stack))
        (push (car stack) result)
        (decf count))
      (setq stack (cdr stack)))
    (nreverse result)))

(defun glx-call-stub-dest-type (call-stub)
  (car call-stub))

(defun glx-call-stub-dest-addr (call-stub)
  (cadr call-stub))

(defun glx-call-stub-pc (call-stub)
  (caddr call-stub))

(defun glx-push-call-stub (dest-type dest-addr)
  "A call stub is a list of three elements, DEST-TYPE and DEST-ADDR 
describe where to put the result of the call, and the third element is 
the address of the next instruction after the one that caused this call
stub to be created. "
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

(defun glx-get-all-local-offsets ()
  "Just used for testing"
  (let (offsets)
    (dolist (local (cadar *glx-stack*) offsets)
      (push (car local) offsets))))

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

(defun glx-push-new-call-frame (locals)
  (glx-stack-push (list nil locals)))

(defun glx-build-call-frame (function-ptr)
  "Build a Glulx VM call frame on the stack, for calling a function at the given 32 bit FUNCTION-PTR."
  (let* ((format-of-locals (glx-get-format-of-locals-from-function-def function-ptr))
         (locals (glx-get-empty-locals-list format-of-locals)))
    (glx-push-new-call-frame locals)))

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

(defun glx-tailcall-function (function-ptr args)
  "Calls the function pointed to by FUNCTION-PTR. Replaces the current call frame 
with a new call frame, does not create a new call stub, but retains the previous one
which will be used when returning from this function."
  (glx-stack-pop)
  (glx-build-call-frame function-ptr)
  (glx-handle-function-args function-ptr args)
  (setq *glx-pc* (glx-get-function-code-start function-ptr)))

(defun glx-return-from-function ()
  "Removes the current call frame from the stack. Also removes and returns the
current call stub. Sets the PC back to the value stored in the call stub."
  (glx-stack-pop)
  (let ((call-stub (glx-stack-pop)))
    (setq *glx-pc* (glx-call-stub-pc call-stub))
    call-stub))

(defun glx-catch-push (token)
  "Pushes a catch token onto the current call frame's stack. This
token is not really on the stack, and is ignored/discarded by all 
stack functions apart from GLX-STACK-UNWIND"
  (glx-value-push (list 'catch token)))

(defun glx-stack-unwind (token)
  "Discards call frames and call stubs and call frame stack values until
a matching catch token is found"

  (let ((not-found t))
    (while (and not-found *glx-stack*)

      (while (and not-found (caar *glx-stack*))
        (let ((val (pop (caar *glx-stack*))))
          (when (and (eq 'catch (first val))
                     (equal token (second val)))
            (setq not-found nil))))

      (when not-found
        (glx-stack-pop) ; discard the call frame
        (glx-stack-pop))) ; discard the call stub 
    
    (if not-found
        (signal 'glx-stack-error (list "Catch token not found" token))
      t)))

(provide 'glx-stack)
