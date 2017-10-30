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
      (glx-instruction-return offset)
    (setq *glx-pc* (glx-- (glx-+ *glx-pc* offset) glx-2))
    (glx-log "Branching - set PC to %x" (glx-32->int *glx-pc*))))

(defmacro glx-def-jump (name opcode args test)
  (let ((fun-name (glx-gen-inst-function name)))
    `(progn
       (glx-defopcode ',name ,opcode ',(make-list (+ 1 (length args)) 'load) #',fun-name)

       (defun ,fun-name
         (,@args offset)
         (when ,test (glx-branch-or-return offset))))))

(defmacro glx-def-store (name opcode args calc &optional bytes)
  (let ((fun-name (glx-gen-inst-function name)))
    `(progn
       (glx-defopcode ',name ,opcode '(,@(make-list (length args) 'load) store) #',fun-name)

       (defun ,fun-name
         (,@args store)
         (funcall (first store) (second store) ,calc ,(if bytes bytes 4))))))

(glx-defopcode 'nop 0 nil #'(lambda () nil))

(glx-def-store add #x10 (l1 l2) (glx-+ l1 l2))
(glx-def-store sub #x11 (l1 l2) (glx-- l1 l2))
(glx-def-store mul #x12 (l1 l2) (glx-* l1 l2))
(glx-def-store div #x13 (l1 l2) (glx-32 (/ (glx-32->int l1) (glx-32->int l2))))
(glx-def-store mod #x14 (l1 l2) (glx-32 (% (glx-32->int l1) (glx-32->int l2))))
(glx-def-store neg #x15 (l1) (glx-- glx-0 l1))
(glx-def-store bitand #x18 (l1 l2) (glx-bitand l1 l2))
(glx-def-store bitor #x19 (l1 l2) (glx-bitor l1 l2))
(glx-def-jump jump #x20 () t)
(glx-def-jump jz #x22 (l1) (glx-0-p l1))
(glx-def-jump jnz #x23 (l1) (not (glx-0-p l1)))
(glx-def-jump jeq #x24 (l1 l2) (equal l1 l2))
(glx-def-jump jne #x25 (l1 l2) (not (equal l1 l2)))
(glx-def-jump jlt #x26 (l1 l2) (glx-neg-p (glx-- l1 l2)))
(glx-def-jump jge #x27 (l1 l2) (not (glx-neg-p (glx-- l1 l2))))
(glx-def-jump jgt #x28 (l1 l2) (glx-pos-p (glx-- l1 l2)))
(glx-def-jump jle #x29 (l1 l2) (not (glx-pos-p (glx-- l1 l2))))
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

(defun glx-instruction-call (fun-ptr arg-count store)
  (let (args)
    (dotimes (i (glx-32->int arg-count))
      (push (glx-value-pop) args))
    (multiple-value-bind (dest-type dest-addr)
        (glx-get-destinations store)
      (glx-call-function fun-ptr dest-type dest-addr (nreverse args)))))

(glx-defopcode 'call #x30 '(load load store) #'glx-instruction-call)

(defun glx-instruction-return (result)
  (let* ((call-stub (glx-return-from-function))
         (dest-type (car call-stub))
         (store-fun (cond ((= 3 dest-type) #'glx-store-stack)
                          ((= 2 dest-type) #'glx-store-local)
                          ((= 1 dest-type) #'glx-store-mem)
                          ((= 0 dest-type) #'glx-store-throw))))
    (funcall store-fun (cadr call-stub) result)))

(glx-defopcode 'return #x31 '(load) #'glx-instruction-return)

(glx-def-store copy #x40 (data) data)
(glx-def-store copys #x41 (data) (glx-32-trunc data 2) 2)
(glx-def-store copyb #x42 (data) (glx-32-trunc data 1) 1)
(glx-def-store aload #x48 (l1 l2) (glx-memory-get-32 (glx-+ l1 (glx-*-byte l2 4))))
(glx-def-store aloads #x49 (l1 l2) (glx-memory-get-16 (glx-+ l1 (glx-*-byte l2 2))))
(glx-def-store aloadb #x4a (l1 l2) (glx-memory-get-byte (glx-+ l1 l2)))

(defun glx-instruction-astore (l1 l2 l3)
  (glx-store-mem (glx-+ l1 (glx-*-byte l2 4)) l3))

(glx-defopcode 'astore #x4c '(load load load) #'glx-instruction-astore)

(defun get-bit-access-vars (l1 l2)
  (let* ((l2int (glx-s32->int l2))
         (l2intmod (% l2int 8)))
    (values (expt 2 (if (< l2intmod 0) (+ 8 l2intmod) l2intmod))
            (glx-+ l1 (glx-32 (+ (/ l2int 8) (if (and (not (zerop l2intmod)) (< l2int 0)) -1 0)))))))

(defun glx-instruction-astoreb (l1 l2 l3)
  (glx-store-mem (glx-+ l1 l2) l3 1))

(glx-defopcode 'astoreb #x4e '(load load load) #'glx-instruction-astoreb)
(glx-def-store aloadbit #x4b (l1 l2)
               (multiple-value-bind (bit-mask memptr) (get-bit-access-vars l1 l2)
                   (if (= 0 (logand bit-mask (glx-memory-get-byte-int memptr))) glx-0 glx-1)))

(defun glx-instruction-astorebit (l1 l2 l3)
  (multiple-value-bind (bit-mask memptr) (get-bit-access-vars l1 l2)
    (glx-memory-set memptr
                    (glx-32 (if (glx-0-p l3)
                                (logand (lognot bit-mask) (glx-memory-get-byte-int memptr))
                              (logior bit-mask (glx-memory-get-byte-int memptr))))
                    1)))

(glx-defopcode 'astorebit #x4f '(load load load) #'glx-instruction-astorebit)

(glx-def-store stkcount #x50 () (glx-stack-count))

(defun glx-instruction-stkcopy (count)
  (dolist (value (reverse (glx-stack-peek (glx-32->int count))))
    (glx-value-push value)))

(glx-defopcode 'stkcopy #x54 '(load) #'glx-instruction-stkcopy)

(glx-defopcode 'streamchar #x70 '(load) #'(lambda (l1) (if *glx-glk-selected* (glk-put-char (glx-32->char l1)))))
(glx-defopcode 'streamnum #x71 '(load) #'(lambda (l1) (if *glx-glk-selected* (glk-put-string (prin1-to-string (glx-s32->int l1))))))
(glx-defopcode 'streamstr #x72 '(load) #'(lambda (l1) (if *glx-glk-selected* (glk-put-string (glx-get-string l1)))))

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
(glx-def-store random #x110 (l1) (glx-32-rand l1))
(glx-defopcode 'quit #x120 '() #'(lambda () 'glx-quit))
(glx-def-store saveundo #x125 () (progn (glx-save-undo) glx-0))
(glx-def-store restoreundo #x126 () (glx-restore-undo))

(glx-def-store glk #x130 (l1 l2) (glx-glk-call (glx-32->int l1) (glx-32->int l2)))

(defun glx-instruction-setiosys (system rock)
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

(defun glx-instruction-callf (fun-ptr store)
  (glx-instruction-type-callf fun-ptr nil store))

(defun glx-instruction-callfi (fun-ptr arg store)
  (glx-instruction-type-callf fun-ptr (list arg) store))

(defun glx-instruction-callfii (fun-ptr arg1 arg2 store)
  (glx-instruction-type-callf fun-ptr (list arg1 arg2) store))

(defun glx-instruction-callfiii (fun-ptr arg1 arg2 arg3 store)
  (glx-instruction-type-callf fun-ptr (list arg1 arg2 arg3) store))

(glx-defopcode 'callf #x160 '(load store) #'glx-instruction-callf)
(glx-defopcode 'callfi #x161 '(load load store) #'glx-instruction-callfi)
(glx-defopcode 'callfii #x162 '(load load load store) #'glx-instruction-callfii)
(glx-defopcode 'callfiii #x163 '(load load load load store) #'glx-instruction-callfiii)

(glx-def-store malloc '#x178 (l1) 0)

(provide 'glx-instructions)
