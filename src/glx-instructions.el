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
(require 'glx-exec)
(require 'glx-string)
(require 'glx-glk)

(defun glx-defopcode (name number arg-list function)
  (puthash number (list name arg-list function) glx-instructions))

(defun glx-branch-or-return (offset)
  (if (or (equal offset glx-0) (equal offset glx-1))
      (glx-instruction-return nil offset)
    (setq *glx-pc* (glx-- (glx-+ *glx-pc* offset) glx-2))
    (glx-log "Branching - set PC to %x" (glx-32->int *glx-pc*))))

(defmacro glx-def-jump (name opcode args test)
  (let ((fun-name (glx-gen-inst-function name)))
    `(progn
       (glx-defopcode ',name ,opcode ',(make-list (+ 1 (length args)) 'load) #',fun-name)

       (defun ,fun-name
         (modes ,@args offset)
         (when ,test (glx-branch-or-return offset))))))

(defmacro glx-def-store (name opcode args calc &optional bytes)
  (let ((fun-name (glx-gen-inst-function name)))
    `(progn
       (glx-defopcode ',name ,opcode '(,@(make-list (length args) 'load) store) #',fun-name)

       (defun ,fun-name
         (modes ,@args store)
         (funcall (first store) (second store) ,calc ,(if bytes bytes 4))))))

(glx-defopcode 'nop 0 nil (lambda (modes) nil))

(glx-def-store add #x10 (l1 l2) (glx-+ l1 l2))
(glx-def-store sub #x11 (l1 l2) (glx-- l1 l2))
(glx-def-store mul #x12 (l1 l2) (glx-* l1 l2))
(glx-def-store div #x13 (l1 l2) (first (glx-/ l1 l2)))
(glx-def-store mod #x14 (l1 l2) (second (glx-/ l1 l2)))
(glx-def-store neg #x15 (l1) (glx-- glx-0 l1))
(glx-def-store bitand #x18 (l1 l2) (glx-bitand l1 l2))
(glx-def-store bitor #x19 (l1 l2) (glx-bitor l1 l2))
(glx-def-store bitxor #x1a (l1 l2) (glx-bitxor l1 l2))
(glx-def-store bitnot #x1b (l1) (glx-bitnot l1))
(glx-def-store shiftl #x1c (l1 l2) (glx-shiftl l1 l2))
(glx-def-store sshiftr #x1d (l1 l2) (glx-sshiftr l1 l2))
(glx-def-store ushiftr #x1e (l1 l2) (glx-ushiftr l1 l2))
(glx-def-jump jump #x20 () t)
(glx-def-jump jz #x22 (l1) (glx-0-p l1))
(glx-def-jump jnz #x23 (l1) (not (glx-0-p l1)))
(glx-def-jump jeq #x24 (l1 l2) (equal l1 l2))
(glx-def-jump jne #x25 (l1 l2) (not (equal l1 l2)))
(glx-def-jump jlt #x26 (l1 l2) (cond ((and (glx-pos-p l1) (or (glx-0-p l2) (glx-neg-p l2))) nil)
                                     ((and (glx-neg-p l1) (or (glx-0-p l2) (glx-pos-p l2))) t)
                                     ((and (glx-0-p l1) (glx-neg-p l2)) nil)
                                     ((and (glx-0-p l1) (glx-pos-p l2)) t)
                                     (t (glx-neg-p (glx-- l1 l2)))))
(glx-def-jump jge #x27 (l1 l2) (cond ((equal l1 l2) t)
                                     ((and (glx-pos-p l1) (or (glx-0-p l2) (glx-neg-p l2))) t)
                                     ((and (glx-neg-p l1) (or (glx-0-p l2) (glx-pos-p l2))) nil)
                                     ((and (glx-0-p l1) (glx-neg-p l2)) t)
                                     ((and (glx-0-p l1) (glx-pos-p l2)) nil)
                                     (t (not (glx-neg-p (glx-- l1 l2))))))
(glx-def-jump jgt #x28 (l1 l2) (cond ((and (glx-pos-p l1) (or (glx-0-p l2) (glx-neg-p l2))) t)
                                     ((and (glx-neg-p l1) (or (glx-0-p l2) (glx-pos-p l2))) nil)
                                     ((and (glx-0-p l1) (glx-neg-p l2)) t)
                                     ((and (glx-0-p l1) (glx-pos-p l2)) nil)
                                     (t (glx-pos-p (glx-- l1 l2)))))
(glx-def-jump jle #x29 (l1 l2) (cond ((equal l1 l2) t)
                                     ((and (glx-pos-p l1) (or (glx-0-p l2) (glx-neg-p l2))) nil)
                                     ((and (glx-neg-p l1) (or (glx-0-p l2) (glx-pos-p l2))) t)
                                     ((and (glx-0-p l1) (glx-neg-p l2)) nil)
                                     ((and (glx-0-p l1) (glx-pos-p l2)) t)
                                     (t (not (glx-pos-p (glx-- l1 l2))))))
(glx-def-jump jltu #x2a (l1 l2) (glx-32-u< l1 l2))
(glx-def-jump jgeu #x2b (l1 l2) (or (glx-32-u> l1 l2) (equal l1 l2)))
(glx-def-jump jgtu #x2c (l1 l2) (glx-32-u> l1 l2))
(glx-def-jump jleu #x2d (l1 l2) (or (glx-32-u< l1 l2) (equal l1 l2)))

(defun glx-get-destinations (store)
  (let ((store-fun (car store)))
    (cond ((eq store-fun #'glx-store-throw)
           (values 0 glx-0))
          ((eq store-fun #'glx-store-mem)
           (values 1 (cadr store)))
          ((eq store-fun #'glx-store-local)
           (values 2 (cadr store)))
          ((eq store-fun #'glx-store-stack)
           (values 3 glx-0)))))

(defun glx-instruction-call (modes fun-ptr arg-count store)
  (let (args)
    (dotimes (i (glx-32->int arg-count))
      (push (glx-value-pop) args))
    (multiple-value-bind (dest-type dest-addr)
        (glx-get-destinations store)
      (glx-call-function fun-ptr dest-type dest-addr (nreverse args)))))

(glx-defopcode 'call #x30 '(load load store) #'glx-instruction-call)

(defun glx-dest-type->store-fun (dest-type)
  (cond ((= 3 dest-type) #'glx-store-stack)
        ((= 2 dest-type) #'glx-store-local)
        ((= 1 dest-type) #'glx-store-mem)
        ((= 0 dest-type) #'glx-store-throw)))

(defun glx-instruction-return (modes result)
  (let* ((call-stub (glx-return-from-function))
         (store-fun (glx-dest-type->store-fun (car call-stub))))
    (funcall store-fun (cadr call-stub) result)))

(glx-defopcode 'return #x31 '(load) #'glx-instruction-return)

(defun glx-instruction-catch (modes store dest)
  (let ((token *glx-catch-token*))
    (setq *glx-catch-token* (glx-+1 token))
    (multiple-value-bind (dest-type dest-addr)
        (glx-get-destinations store)
      (glx-value-push (glx-32 dest-type))
      (glx-value-push dest-addr))
    (glx-value-push *glx-pc*)
    (glx-value-push glx-0)  ; frame-ptr (we don't have one)
    (glx-value-push (list 'catch token))
    (funcall (first store) (second store) token)
    (glx-branch-or-return dest)))

(glx-defopcode 'catch #x32 '(store load) #'glx-instruction-catch)

(defun glx-instruction-throw (modes value token)
  (glx-stack-unwind token)
  (glx-value-pop) ;; discard frame pointer
  (setq *glx-pc* (glx-value-pop))
  (let ((dest-addr (glx-value-pop))
        (store-fun (glx-dest-type->store-fun (car (last (glx-32-get-bytes-as-list-big-endian (glx-value-pop)))))))
    (funcall store-fun dest-addr value)))

(glx-defopcode 'throw #x33 '(load load) #'glx-instruction-throw)

(defun glx-instruction-tailcall (modes fun-ptr arg-count)
  (let (args)
    (dotimes (i (glx-32->int arg-count))
      (push (glx-value-pop) args))
    (glx-tailcall-function fun-ptr (nreverse args))))

(glx-defopcode 'tailcall #x34 '(load load) #'glx-instruction-tailcall)

(glx-def-store copy #x40 (data) data)
(glx-def-store copys #x41 (data) (if (or (= (first modes) 3) (= (first modes) 8))
                                     (glx-32-lo-trunc data 2)
                                   (glx-32-hi-trunc data 2)) 2)
(glx-def-store copyb #x42 (data) (if (or (= (first modes) 3) (= (first modes) 8))
                                     (glx-32-lo-trunc data 1)
                                   (glx-32-hi-trunc data 1)) 1)
(glx-def-store sexs #x44 (data) (let ((bytes (glx-32-get-bytes-as-list-big-endian data)))
                                  (if (zerop (logand (third bytes) 128))
                                      (glx-32 (fourth bytes) (third bytes) 0 0)
                                    (glx-32 (fourth bytes) (third bytes) 255 255))))
(glx-def-store sexb #x45 (data) (let ((bytes (glx-32-get-bytes-as-list-big-endian data)))
                                  (if (zerop (logand (fourth bytes) 128))
                                      (glx-32 (fourth bytes) 0 0 0)
                                    (glx-32 (fourth bytes) 255 255 255))))
(glx-def-store aload #x48 (l1 l2) (glx-memory-get-32 (glx-+ l1 (glx-*-byte l2 4))))
(glx-def-store aloads #x49 (l1 l2) (glx-memory-get-16 (glx-+ l1 (glx-*-byte l2 2))))
(glx-def-store aloadb #x4a (l1 l2) (glx-memory-get-byte (glx-+ l1 l2)))

(defun get-bit-access-vars (l1 l2)
  (let* ((l2int (glx-s32->int l2))
         (l2intmod (% l2int 8)))
    (values (expt 2 (if (< l2intmod 0) (+ 8 l2intmod) l2intmod))
            (glx-+ l1 (glx-32 (+ (/ l2int 8) (if (and (not (zerop l2intmod)) (< l2int 0)) -1 0)))))))

(glx-def-store aloadbit #x4b (l1 l2)
               (multiple-value-bind (bit-mask memptr) (get-bit-access-vars l1 l2)
                 (if (= 0 (logand bit-mask (glx-memory-get-byte-int memptr))) glx-0 glx-1)))

(defun glx-instruction-astore (modes l1 l2 l3)
  (glx-store-mem (glx-+ l1 (glx-*-byte l2 4)) l3))

(glx-defopcode 'astore #x4c '(load load load) #'glx-instruction-astore)

(defun glx-instruction-astores (modes l1 l2 l3)
  (glx-store-mem (glx-+ l1 (glx-*-byte l2 2)) l3 2))

(glx-defopcode 'astores #x4d '(load load load) #'glx-instruction-astores)

(defun glx-instruction-astoreb (modes l1 l2 l3)
  (glx-store-mem (glx-+ l1 l2) l3 1))

(glx-defopcode 'astoreb #x4e '(load load load) #'glx-instruction-astoreb)

(defun glx-instruction-astorebit (modes l1 l2 l3)
  (multiple-value-bind (bit-mask memptr) (get-bit-access-vars l1 l2)
    (glx-memory-set memptr
                    (glx-32 (if (glx-0-p l3)
                                (logand (lognot bit-mask) (glx-memory-get-byte-int memptr))
                              (logior bit-mask (glx-memory-get-byte-int memptr))))
                    1)))

(glx-defopcode 'astorebit #x4f '(load load load) #'glx-instruction-astorebit)

(glx-def-store stkcount #x50 () (glx-stack-count))

(glx-def-store stkpeek #x51 (idx) (car (last (glx-stack-peek (+ 1 (glx-32->int idx))))))

(defun glx-instruction-stkswap (modes)
  (let ((top (glx-value-pop))
        (next (glx-value-pop)))
    (glx-value-push top)
    (glx-value-push next)))

(glx-defopcode 'stkswap #x52 () #'glx-instruction-stkswap)

(defun glx-instruction-stkroll (modes vals rot)
  (when (and (not (glx-0-p vals)) (not (glx-0-p rot)))
    (let (popped
          (left (glx-neg-p rot))
          (rotmod (glx-abs (second (glx-/ rot vals)))))
      (dotimes (n (glx-32->int vals))
        (push (glx-value-pop) popped))
      (while (glx-32-u> rotmod glx-0)
        (if left
            (setq popped (append (cdr popped) (list (car popped))))
          (setq popped (cons (car (last popped)) (butlast popped))))
        (setq rotmod (glx-- rotmod glx-1)))
      (dolist (elt popped)
        (glx-value-push elt)))))

(glx-defopcode 'stkroll #x53 '(load load) #'glx-instruction-stkroll)

(defun glx-instruction-stkcopy (modes count)
  (dolist (value (reverse (glx-stack-peek (glx-32->int count))))
    (glx-value-push value)))

(glx-defopcode 'stkcopy #x54 '(load) #'glx-instruction-stkcopy)

(glx-defopcode 'streamchar #x70 '(load) (lambda (modes l1) (if *glx-glk-selected* (glk-put-char (glx-32->char l1)))))
(glx-defopcode 'streamnum #x71 '(load) (lambda (modes l1) (if *glx-glk-selected* (glk-put-string (glx-32->dec-string l1)))))
(glx-defopcode 'streamstr #x72 '(load) (lambda (modes l1) (if *glx-glk-selected* (glk-put-string (glx-get-string l1)))))

(glx-def-store gestalt #x100 (l1 l2) (cond ((equal glx-0 l1) (glx-32 0 1 3))
                                           ((equal glx-1 l1) glx-0)
                                           ((equal glx-2 l1) glx-1)
                                           ((equal glx-3 l1) glx-0)
                                           ((equal glx-4 l1) (if (or (equal l2 glx-0)
                                                                     (equal l2 glx-1)
                                                                     (equal l2 glx-2))
                                                                 glx-1
                                                               glx-0))))

(glx-def-store getmemsize #x102 () (glx-32 (length *glx-memory*)))

(defun glx-instruction-jumpabs (modes dest)
  (setq *glx-pc* dest)
  (glx-log "Branching - set PC to %x" (glx-32->int *glx-pc*)))

(glx-defopcode 'jumpabs #x104 '(load) #'glx-instruction-jumpabs)
(glx-def-store random #x110 (l1) (glx-32-rand l1))
(glx-defopcode 'quit #x120 '() (lambda (modes) 'glx-quit))
(glx-def-store saveundo #x125 () (progn (glx-save-undo) glx-0))
(glx-def-store restoreundo #x126 () (glx-restore-undo))

(glx-def-store glk #x130 (l1 l2) (glx-glk-call (glx-32->int l1) (glx-32->int l2)))

(glx-def-store getstringtbl #x140 () *glx-string-table*)
(glx-defopcode 'setstringtbl #x141 '(load) (lambda (modes table) (setq *glx-string-table* table)))

(defun glx-instruction-setiosys (modes system rock)
  (setq *glx-glk-selected* (equal system glx-2))
  (if (not (or (equal system glx-2) (equal system glx-0)))
      (signal 'glx-glk-error (list "Unknown io system" system))))

(glx-defopcode 'setiosys #x149 '(load load) #'glx-instruction-setiosys)

(glx-def-store linearsearch #x150 (l1 l2 l3 l4 l5 l6 l7) (glx-memory-linear-search l1 l2 l3 l4 l5 l6 l7))
(glx-def-store binarysearch #x151 (l1 l2 l3 l4 l5 l6 l7) (glx-memory-binary-search l1 l2 l3 l4 l5 l6 l7))

(defun glx-instruction-type-callf (fun-ptr arg-list store)
  (multiple-value-bind (dest-type dest-addr)
      (glx-get-destinations store)
    (glx-call-function fun-ptr dest-type dest-addr arg-list)))

(defun glx-instruction-callf (modes fun-ptr store)
  (glx-instruction-type-callf fun-ptr nil store))

(defun glx-instruction-callfi (modes fun-ptr arg store)
  (glx-instruction-type-callf fun-ptr (list arg) store))

(defun glx-instruction-callfii (modes fun-ptr arg1 arg2 store)
  (glx-instruction-type-callf fun-ptr (list arg1 arg2) store))

(defun glx-instruction-callfiii (modes fun-ptr arg1 arg2 arg3 store)
  (glx-instruction-type-callf fun-ptr (list arg1 arg2 arg3) store))

(glx-defopcode 'callf #x160 '(load store) #'glx-instruction-callf)
(glx-defopcode 'callfi #x161 '(load load store) #'glx-instruction-callfi)
(glx-defopcode 'callfii #x162 '(load load load store) #'glx-instruction-callfii)
(glx-defopcode 'callfiii #x163 '(load load load load store) #'glx-instruction-callfiii)

(glx-def-store malloc '#x178 (l1) 0)

(provide 'glx-instructions)
