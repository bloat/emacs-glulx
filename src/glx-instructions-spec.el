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

(defun verify-glx-stack (&rest vals)
  (dolist (v (nreverse vals))
    (should (equal (glx-value-pop) (glx-32 v))))
  (should (equal (glx-stack-count) glx-0)))

(defmacro with-glx-stack (start end &rest body)
  (declare (indent 2))
  `(let ((*glx-stack* ()))
     (glx-push-new-call-frame ())
     (dolist (v (list ,@start)) (glx-value-push (glx-32 v)))
     ,@body
     (verify-glx-stack ,@end))) 

(defmacro with-glx-locals (start end &rest body)
  (declare (indent 2))
  (let ((locals ())
        (checks ()))
    (dolist (l start) (push `(cons (glx-32 ,(car l)) (glx-32 ,(cadr l))) locals))
    (dolist (c end) (push `(should (equal (glx-get-local-at-offset (glx-32 ,(car c))) (glx-32 ,(cadr c)))) checks))
    `(let ((*glx-stack* ()))
       (glx-push-new-call-frame (list ,@locals))
       ,@body
       ,@checks)))

(defmacro with-glx-memory (start end &rest body)
  (declare (indent 2))
  `(let ((*glx-memory* (vector ,@start)))
     ,@body
     (should (equal *glx-memory* (vector ,@end)))))

(ert-deftest call-instruction ()
  "call instruction"
  :tags '(instructions)
  (let ((call-args nil))

    (cl-letf (((symbol-function 'glx-call-function) (lambda (fptr dt da args) (setq call-args (list fptr dt da args)))))

      (glx-instruction-call nil glx-0 glx-0 (list #'glx-store-throw nil))
      (should (equal call-args (list glx-0 0 glx-0 nil)))

      ;; two args call - store result in memory
      (with-glx-stack (3 2) ()
        (glx-instruction-call nil glx-0 glx-2 (list #'glx-store-mem glx-4))
        (should (equal call-args (list glx-0 1 glx-4 (list glx-2 glx-3)))))
      
      ;; zero args call - push result
      (glx-instruction-call nil glx-0 glx-0 (list #'glx-store-stack nil))
      (should (equal call-args (list glx-0 3 glx-0 nil)))

      ;; zero args call - store result in a local
      (glx-instruction-call nil glx-0 glx-0 (list #'glx-store-local glx-5))
      (should (equal call-args (list glx-0 2 glx-5 nil))))))

(ert-deftest tailcall-instruction ()
  "tailcall instruction"
  :tags '(instructions)
  (let ((call-args nil))

    (cl-letf (((symbol-function 'glx-tailcall-function) (lambda (fptr args) (setq call-args (list fptr args)))))

      (glx-instruction-tailcall nil glx-0 glx-0)
      (should (equal call-args (list glx-0 nil)))

      ;; two args call
      (with-glx-stack (3 2) ()
        (glx-instruction-tailcall nil glx-0 glx-2)
        (should (equal call-args (list glx-0 (list glx-2 glx-3)))))

      ;; zero args call
      (glx-instruction-tailcall nil glx-0 glx-0)
      (should (equal call-args (list glx-0 nil))))))

(ert-deftest copy-instruction ()
  "copy instruction"
  :tags '(instructions)

  ;; copy to stack
  (with-glx-stack () (2)
    (glx-instruction-copy nil glx-2 (list #'glx-store-stack nil)))

  ;; copy to local
  (with-glx-locals ((2 0)) ((2 2))
    (glx-instruction-copy nil glx-2 (list #'glx-store-local glx-2))))

(ert-deftest sub-instruction ()
  "sub instruction"
  :tags '(instructions)

  ;; store to mem
  (with-glx-memory (0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 1 0 0 0 0)
    (glx-instruction-sub nil glx-2 glx-1 (list #'glx-store-mem glx-4))))

(ert-deftest jlt-instruction ()
  "jlt instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-0))
    (glx-instruction-jlt nil glx-2 glx-1 glx-5)
    (should (equal *glx-pc* glx-0))
    (glx-instruction-jlt nil glx-1 glx-2 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jlt nil glx-1 glx-1 glx-8)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jlt nil (glx-32 -1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jlt nil (glx-32 -2) (glx-32 -1) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jlt nil (glx-32 -2) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jlt nil (glx-32 -2) (glx-32 1) glx-8)
    (should (equal *glx-pc* (glx-32 15)))
    (glx-instruction-jlt nil glx-2 (glx-32 255 255 255 127) glx-8)
    (should (equal *glx-pc* (glx-32 21)))
    (glx-instruction-jlt nil glx-2 (glx-32 0 0 0 128) glx-8)
    (should (equal *glx-pc* (glx-32 21)))
    (glx-instruction-jlt nil (glx-32 -2) (glx-32 255 255 255 127) glx-8)
    (should (equal *glx-pc* (glx-32 27)))
    (glx-instruction-jlt nil (glx-32 -2) (glx-32 0 0 0 128) glx-8)
    (should (equal *glx-pc* (glx-32 27))))

  (let (return-was-called)
    (cl-letf (((symbol-function 'glx-return-from-function) (lambda (result) (setq return-was-called result))))
      (glx-instruction-jlt nil glx-1 glx-2 glx-1)
      (should (equal return-was-called glx-1)))))

(ert-deftest getmemsize-instruction ()
  "getmemsize instruction"
  :tags '(instructions)

  (with-glx-memory (0 0 0 0 0 0 0 0) (0 0 0 0 8 0 0 0)
    (glx-instruction-getmemsize nil (list #'glx-store-mem glx-1))))

(ert-deftest jne-instruction ()
  "jne instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-4))
    (glx-instruction-jne nil glx-2 glx-1 glx-5)
    (should (equal *glx-pc* (glx-32 7)))
    (glx-instruction-jne nil glx-1 glx-1 glx-5)
    (should (equal *glx-pc* (glx-32 7))))

  (let (return-was-called)
    (cl-letf (((symbol-function 'glx-return-from-function) (lambda (result) (setq return-was-called result))))
      (glx-instruction-jne nil glx-1 glx-2 glx-0)
      (should (equal return-was-called glx-0)))))

(ert-deftest jge-instruction ()
  "jge instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-0))
    (glx-instruction-jge nil glx-2 glx-1 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jge nil glx-1 glx-2 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jge nil glx-1 glx-1 glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jge nil (glx-32 -1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 15)))
    (glx-instruction-jge nil (glx-32 -2) (glx-32 -1) glx-8)
    (should (equal *glx-pc* (glx-32 15)))
    (glx-instruction-jge nil (glx-32 -2) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 21)))
    (glx-instruction-jge nil (glx-32 1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 27)))
    (glx-instruction-jge nil glx-2 (glx-32 255 255 255 127) glx-8)
    (should (equal *glx-pc* (glx-32 27)))
    (glx-instruction-jge nil glx-2 (glx-32 0 0 0 128) glx-8)
    (should (equal *glx-pc* (glx-32 33)))
    (glx-instruction-jge nil (glx-32 -2) (glx-32 255 255 255 127) glx-8)
    (should (equal *glx-pc* (glx-32 33)))
    (glx-instruction-jge nil (glx-32 -2) (glx-32 0 0 0 128) glx-8)
    (should (equal *glx-pc* (glx-32 39)))))

(ert-deftest jle-instruction ()
  "jle instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-0))
    (glx-instruction-jle nil glx-2 glx-1 glx-5)
    (should (equal *glx-pc* glx-0))
    (glx-instruction-jle nil glx-1 glx-2 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jle nil glx-1 glx-1 glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jle nil (glx-32 -1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jle nil (glx-32 -2) (glx-32 -1) glx-8)
    (should (equal *glx-pc* (glx-32 15)))
    (glx-instruction-jle nil (glx-32 -2) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 21)))
    (glx-instruction-jle nil (glx-32 1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 21)))
    (glx-instruction-jle nil glx-2 (glx-32 255 255 255 127) glx-8)
    (should (equal *glx-pc* (glx-32 27)))
    (glx-instruction-jle nil glx-2 (glx-32 0 0 0 128) glx-8)
    (should (equal *glx-pc* (glx-32 27)))
    (glx-instruction-jle nil (glx-32 -2) (glx-32 255 255 255 127) glx-8)
    (should (equal *glx-pc* (glx-32 33)))
    (glx-instruction-jle nil (glx-32 -2) (glx-32 0 0 0 128) glx-8)
    (should (equal *glx-pc* (glx-32 33)))))

(ert-deftest aloadb-instruction ()
  "aloadb instruction"
  :tags '(instructions)

  (with-glx-memory (0 1 2 3 4 5 6 7 8) (0 1 2 3 0 0 0 3 8)
    (glx-instruction-aloadb nil glx-2 glx-1 (list #'glx-store-mem glx-4))))

(ert-deftest jgt-instruction ()
  "jgt instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-0))
    (glx-instruction-jgt nil glx-2 glx-1 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jgt nil glx-1 glx-2 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jgt nil glx-1 glx-1 glx-8)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jgt nil (glx-32 -1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jgt nil (glx-32 -2) (glx-32 -1) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jgt nil (glx-32 -2) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jgt nil (glx-32 1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 15)))
    (glx-instruction-jgt nil glx-2 (glx-32 255 255 255 127) glx-8)
    (should (equal *glx-pc* (glx-32 15)))
    (glx-instruction-jgt nil glx-2 (glx-32 0 0 0 128) glx-8)
    (should (equal *glx-pc* (glx-32 21)))
    (glx-instruction-jgt nil (glx-32 -2) (glx-32 255 255 255 127) glx-8)
    (should (equal *glx-pc* (glx-32 21)))
    (glx-instruction-jgt nil (glx-32 -2) (glx-32 0 0 0 128) glx-8)
    (should (equal *glx-pc* (glx-32 27)))))

(ert-deftest aload-instruction ()
  "aload instruction"
  :tags '(instructions)

  (with-glx-memory (0 1 2 3 4 5 0 0 8 9 10) (0 1 2 3 4 5 0 0 8 9 10)
    (with-glx-stack () (2057)
      (glx-instruction-aload nil glx-2 glx-1 (list #'glx-store-stack nil)))))

(ert-deftest jeq-instruction ()
  "jeq instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-0))
    (glx-instruction-jeq nil glx-2 glx-1 glx-5)
    (should (equal *glx-pc* glx-0))
    (glx-instruction-jeq nil glx-1 glx-2 glx-5)
    (should (equal *glx-pc* glx-0))
    (glx-instruction-jeq nil glx-1 glx-1 glx-8)
    (should (equal *glx-pc* (glx-32 6)))
    (glx-instruction-jeq nil (glx-32 -1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 6)))
    (glx-instruction-jeq nil (glx-32 -2) (glx-32 -1) glx-8)
    (should (equal *glx-pc* (glx-32 6)))
    (glx-instruction-jeq nil (glx-32 -2) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 12)))))

(ert-deftest jz-instruction ()
  "jz instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-0))
    (glx-instruction-jz nil glx-0 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jz nil glx-1 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jz nil (glx-32 -1) glx-8)
    (should (equal *glx-pc* glx-3))))

(ert-deftest jnz-instruction ()
  "jnz instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-0))
    (glx-instruction-jnz nil glx-0 glx-5)
    (should (equal *glx-pc* glx-0))
    (glx-instruction-jnz nil glx-1 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jnz nil (glx-32 -1) glx-8)
    (should (equal *glx-pc* (glx-32 9)))))

(ert-deftest jumpabs-instruction ()
  "jumpabs instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-5))
    (glx-instruction-jumpabs nil glx-8)
    (should (equal *glx-pc* glx-8))
    (glx-instruction-jumpabs nil glx-0)
    (should (equal *glx-pc* glx-0))
    (glx-instruction-jumpabs nil glx-1)
    (should (equal *glx-pc* glx-1))))

(ert-deftest add-instruction ()
  "add instruction"
  :tags '(instructions)

  ;; store to mem
  (with-glx-memory (0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 13 0 0 0 0)
    (glx-instruction-add nil glx-5 glx-8 (list #'glx-store-mem glx-4))))

(ert-deftest aloadbit-instruction ()
  "aloadbit instruction"
  :tags '(instructions)

  ;; store to mem
  (with-glx-memory (130 1 0 0 255 255 255 255) (130 1 0 0 0 0 0 0)
    (glx-instruction-aloadbit nil glx-0 glx-0 (list #'glx-store-mem glx-4)))
  (with-glx-memory (130 1 0 0 0 0 0 0) (130 1 0 0 0 0 0 1)
    (glx-instruction-aloadbit nil glx-0 glx-1 (list #'glx-store-mem glx-4)))
  (with-glx-memory (130 1 0 0 255 255 255 255) (130 1 0 0 0 0 0 1)
    (glx-instruction-aloadbit nil glx-0 (glx-32 7) (list #'glx-store-mem glx-4)))
  (with-glx-memory (130 1 0 0 255 255 255 255) (130 1 0 0 0 0 0 1)
    (glx-instruction-aloadbit nil glx-0 glx-8 (list #'glx-store-mem glx-4)))
  (with-glx-memory (130 1 0 0 0 0 0 1) (130 1 0 0 0 0 0 0)
    (glx-instruction-aloadbit nil glx-0 (glx-32 9) (list #'glx-store-mem glx-4)))
  (with-glx-memory (130 1 0 0 0 0 0 0) (130 1 0 0 0 0 0 1)
    (glx-instruction-aloadbit nil glx-1 (glx-32 -1) (list #'glx-store-mem glx-4)))
  (with-glx-memory (130 1 0 0 0 0 0 1) (130 1 0 0 0 0 0 0)
    (glx-instruction-aloadbit nil glx-1 (glx-32 -2) (list #'glx-store-mem glx-4)))
  (with-glx-memory (130 1 0 0 0 0 0 0) (130 1 0 0 0 0 0 1)
    (glx-instruction-aloadbit nil glx-1 (glx-32 -7) (list #'glx-store-mem glx-4)))
  (with-glx-memory (130 1 0 0 255 255 255 255) (130 1 0 0 0 0 0 0)
    (glx-instruction-aloadbit nil glx-1 (glx-32 -8) (list #'glx-store-mem glx-4)))
  (with-glx-memory (130 1 0 0 0 0 0 0) (130 1 0 0 0 0 0 1)
    (glx-instruction-aloadbit nil glx-2 (glx-32 -9) (list #'glx-store-mem glx-4)))
  (with-glx-memory (0 0 32 0 0 0 0 0) (0 0 32 0 0 0 0 1)
    (glx-instruction-aloadbit nil glx-0 (glx-32 21) (list #'glx-store-mem glx-4)))
  (with-glx-memory (0 0 32 0 255 255 255 255) (0 0 32 0 0 0 0 1)
    (glx-instruction-aloadbit nil (glx-32 6) (glx-32 -27) (list #'glx-store-mem glx-4))))

(ert-deftest aloads-instruction ()
  "aloads instruction"
  :tags '(instructions)
  (with-glx-memory (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
    (with-glx-stack () (258)
      (glx-instruction-aloads nil glx-0 glx-0 (list #'glx-store-stack nil)))
    (with-glx-stack () (1800)
      (glx-instruction-aloads nil glx-0 glx-3 (list #'glx-store-stack nil)))
    (with-glx-stack () (1800)
      (glx-instruction-aloads nil glx-4 glx-1 (list #'glx-store-stack nil)))))

(ert-deftest mul-instruction ()
  "mul instruction"
  :tags '(instructions)

  ;; store to mem
  (with-glx-memory (0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 40 0 0 0 0)
    (glx-instruction-mul nil glx-5 glx-8 (list #'glx-store-mem glx-4))))

(ert-deftest stkpeek-instruction ()
  "stkpeek instruction"
  :tags '(instructions)

  (with-glx-memory (0 0 0 0 0) (0 0 0 0 3)
    (with-glx-stack (3 2 1 -1) (3 2 1 -1 -1)
      (glx-instruction-stkpeek nil glx-3 (list #'glx-store-mem glx-1))
      (glx-instruction-stkpeek nil glx-0 (list #'glx-store-stack nil)))))

(ert-deftest stkswap-instruction ()
  "stkswap instruction"
  :tags '(instructions)

  (with-glx-stack (3 2 1 -1) (3 2 -1 1)
    (glx-instruction-stkswap nil)))

(ert-deftest stkroll-instruction ()
  "stkroll instruction"
  :tags '(instructions)

  (with-glx-stack (8 7 6 5 4 3 2 1 0) (8 7 6 5 0 4 3 2 1)
    (glx-instruction-stkroll nil glx-5 glx-1))
  (with-glx-stack (8 7 6 5 0 4 3 2 1) (5 0 4 3 2 1 8 7 6)
    (glx-instruction-stkroll nil (glx-32 9) (glx-32 -3)))
  (with-glx-stack (5 0 4 3 2 1 8 7 6) (5 0 4 3 2 1 8 7 6)
    (glx-instruction-stkroll nil glx-0 glx-1)))

(ert-deftest stkcopy-instruction ()
  "stkcopy instruction"
  :tags '(instructions)

  (with-glx-stack (3 2 1 0) (3 2 1 0 2 1 0)
    (glx-instruction-stkcopy nil glx-3)))

(ert-deftest gestalt-instruction ()
  "gestalt instruction"
  :tags '(instructions)

  (with-glx-memory (0 0 0 0 0) (0 0 3 1 3)
    (glx-instruction-gestalt nil glx-0 glx-0 (list #'glx-store-mem glx-1)))
  (with-glx-memory (0 0 3 1 0) (0 0 0 1 0)
    (glx-instruction-gestalt nil glx-1 glx-0 (list #'glx-store-mem glx-1)))
  (with-glx-memory (0 0 0 0 0) (0 0 0 0 1)
    (glx-instruction-gestalt nil glx-2 glx-0 (list #'glx-store-mem glx-1)))
  (with-glx-memory (0 0 0 0 0) (0 0 0 0 1)
    (glx-instruction-gestalt nil glx-3 glx-0 (list #'glx-store-mem glx-1)))
  (with-glx-memory (0 0 0 0 0) (0 0 0 0 1)
    (glx-instruction-gestalt nil glx-4 glx-0 (list #'glx-store-mem glx-1)))
  (with-glx-memory (0 0 0 0 1) (0 0 0 0 1)
    (glx-instruction-gestalt nil glx-4 glx-2 (list #'glx-store-mem glx-1)))
  (with-glx-memory (0 0 0 0 1) (0 0 0 0 1)
    (glx-instruction-gestalt nil glx-4 glx-2 (list #'glx-store-mem glx-1)))
  (with-glx-memory (0 0 0 0 1) (0 0 0 0 0)
    (glx-instruction-gestalt nil glx-4 glx-3 (list #'glx-store-mem glx-1))))

(ert-deftest setiosys ()
  "setiosys"
  :tags '(instructions)
  (let ((*glx-iosys* nil))
    (glx-instruction-setiosys nil glx-2 (glx-32 14))
    (should (glx-iosys-charfun))
    (should (equal (glx-32 14) (glx-iosys-rock)))
    
    (should-error (glx-instruction-setiosys nil glx-3 glx-0) :type 'glx-glk-error)))

(ert-deftest jump-instruction ()
  "jump instruction"
  :tags '(instructions)
  (let ((*glx-pc* glx-0))
    (glx-instruction-jump nil glx-5)
    (should (equal *glx-pc* glx-3))))

(ert-deftest astore-instruction ()
  "astore instruction"
  :tags '(instructions)
  (with-glx-memory (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8)
    (glx-instruction-astore nil glx-4 glx-2 glx-8)))

(ert-deftest astores-instruction ()
  "astores instruction"
  :tags '(instructions)
  (with-glx-memory (0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 8)
    (glx-instruction-astores nil glx-4 glx-2 glx-8)))

(ert-deftest copys-instruction ()
  "copys instruction"
  :tags '(instructions)

  ;; copy to stack - constant - from a 4 byte constant copys takes the lowest 2 bytes
  (with-glx-stack () (1543)
    (glx-instruction-copys '(3 8) (glx-32 7 6 5 4) (list #'glx-store-stack nil)))

  ;; copy to memory - constant - from a 4 byte constant copys takes the lowest 2 bytes
  (with-glx-memory (0 0 0 0) (0 0 8 9)
    (glx-instruction-copys '(3 7) (glx-32 9 8 7 6) (list #'glx-store-mem glx-2)))

  ;; copy to memory - 2 bytes popped from stack - the system pops a 4 byte value, copys
  ;; takes the lowest 2 bytes.
  (with-glx-memory (0 0 0 0) (0 0 8 9)
    (glx-instruction-copys '(8 7) (glx-32 9 8 7 6) (list #'glx-store-mem glx-2)))

  ;; copy to stack - 2 bytes loaded from memory - the system provides 4 bytes, but we want
  ;; the 2 bytes at the given memory address, i.e. the top two bytes 
  (with-glx-stack () (1029)
    (glx-instruction-copys '(#xe 8) (glx-32 7 6 5 4) (list #'glx-store-stack nil)))

  ;; copy to memory - 2 bytes loaded from memory - the system provides 4 bytes, but we want
  ;; the 2 bytes at the given memory address, i.e. the top two bytes
  (with-glx-memory (0 0 0 0) (0 0 6 7)
    (glx-instruction-copys '(7 7) (glx-32 9 8 7 6) (list #'glx-store-mem glx-2))))

(ert-deftest copyb-instruction ()
  "copyb instruction"
  :tags '(instructions)

  ;; copy to stack - constant - from a 4 byte constant copyb takes the lowest byte
  (with-glx-stack () (7)
    (glx-instruction-copyb '(3 8) (glx-32 7 6 5 4) (list #'glx-store-stack nil)))

  ;; copy to memory - constant - from a 4 byte constant copyb takes the lowest byte
  (with-glx-memory (0 0 0 0) (0 0 9 0)
    (glx-instruction-copyb '(3 7) (glx-32 9 8 7 6) (list #'glx-store-mem glx-2)))

  ;; copy to memory - 1 byte popped from stack - the system pops a 4 byte value, copyb
  ;; takes the lowest byte.
  (with-glx-memory (0 0 0 0) (0 0 9 0)
    (glx-instruction-copyb '(8 7) (glx-32 9 8 7 6) (list #'glx-store-mem glx-2)))

  ;; copy to stack - 1 byte loaded from memory - the system provides 4 bytes, but we want
  ;; the byte at the given memory address, i.e. the top byte
  (with-glx-stack () (4)
    (glx-instruction-copyb '(#xe 8) (glx-32 7 6 5 4) (list #'glx-store-stack nil)))

  ;; copy to memory - 1 byte loaded from memory - the system provides 4 bytes, but we want
  ;; the byte at the given memory address, i.e. the top byte
  (with-glx-memory (0 0 0 0) (0 0 6 0)
    (glx-instruction-copyb '(7 7) (glx-32 9 8 7 6) (list #'glx-store-mem glx-2))))

(ert-deftest astorebit-instruction ()
  "astorebit instruction"
  :tags '(instructions)

  ;; store to mem
  (with-glx-memory (130 1 0 0) (131 1 0 0)
    (glx-instruction-astorebit nil glx-0 glx-0 glx-1))
  (with-glx-memory (131 1 0 0) (130 1 0 0)
    (glx-instruction-astorebit nil glx-0 glx-0 glx-0))
  (with-glx-memory (130 1 0 0) (2 1 0 0)
    (glx-instruction-astorebit nil glx-0 (glx-32 7) glx-0))
  (with-glx-memory (2 1 0 0) (130 1 0 0)
    (glx-instruction-astorebit nil glx-0 (glx-32 7) glx-1))
  (with-glx-memory (130 1 0 0) (130 1 0 0)
    (glx-instruction-astorebit nil glx-0 (glx-32 9) glx-0))
  (with-glx-memory (130 1 0 0) (2 1 0 0)
    (glx-instruction-astorebit nil glx-1 (glx-32 -1) glx-0))
  (with-glx-memory (2 1 0 0) (66 1 0 0)
    (glx-instruction-astorebit nil glx-1 (glx-32 -2) glx-1))
  (with-glx-memory (66 1 0 0) (64 1 0 0)
    (glx-instruction-astorebit nil glx-1 (glx-32 -7) glx-0))
  (with-glx-memory (64 1 0 0) (65 1 0 0)
    (glx-instruction-astorebit nil glx-1 (glx-32 -8) glx-1))
  (with-glx-memory (65 1 0 0) (193 1 0 0)
    (glx-instruction-astorebit nil glx-2 (glx-32 -9) glx-1))
  (with-glx-memory (193 1 0 0) (193 1 32 0)
    (glx-instruction-astorebit nil glx-0 (glx-32 21) glx-1)))

(ert-deftest astoreb-instruction ()
  "astoreb instruction"
  :tags '(instructions)
  (with-glx-memory (0 1 2 3 4 5 6 7 8) (0 1 2 4 4 5 6 7 8)
    (glx-instruction-astoreb nil glx-1 glx-2 (glx-32 4 5 6 7))))

(ert-deftest callf-instruction ()
  "callf instruction"
  :tags '(instructions)

  (let ((call-args nil))
    (cl-letf (((symbol-function 'glx-call-function) (lambda (fptr dt da args) (setq call-args (list fptr dt da args)))))

      ;; store result in memory
      (glx-instruction-callf nil glx-1 (list #'glx-store-mem glx-4))
      (should (equal call-args (list glx-1 1 glx-4 nil))))))

(ert-deftest callfi-instruction ()
  "callfi instruction"
  :tags '(instructions)
  (let ((call-args nil))
    (cl-letf (((symbol-function 'glx-call-function) (lambda (fptr dt da args) (setq call-args (list fptr dt da args)))))

      ;; store result in memory
      (glx-instruction-callfi nil glx-1 glx-4 (list #'glx-store-mem glx-3))
      (should (equal call-args (list glx-1 1 glx-3 (list glx-4)))))))

(ert-deftest callfii-instruction ()
  "callfii instruction"
  :tags '(instructions)
  (let ((call-args nil))
    (cl-letf (((symbol-function 'glx-call-function) (lambda  (fptr dt da args) (setq call-args (list fptr dt da args)))))

      ;; store result in memory
      (glx-instruction-callfii nil glx-1 glx-2 glx-3 (list #'glx-store-mem glx-8))
      (should (equal call-args (list glx-1 1 glx-8 (list glx-2 glx-3)))))))

(ert-deftest callfiii-instruction ()
  "callfiii instruction"
  :tags '(instructions)
  (let ((call-args nil))
    (cl-letf (((symbol-function 'glx-call-function) (lambda (fptr dt da args) (setq call-args (list fptr dt da args)))))

      ;; store result in memory
      (glx-instruction-callfiii nil glx-0 glx-5 glx-4 glx-3 (list #'glx-store-mem glx-4))
      (should (equal call-args (list glx-0 1 glx-4 (list glx-5 glx-4 glx-3)))))))

(ert-deftest jgeu-instruction ()
  "jgeu instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-0))
    (glx-instruction-jgeu nil glx-2 glx-1 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jgeu nil glx-1 glx-2 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jgeu nil glx-1 glx-1 glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jgeu nil (glx-32 -1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 15)))
    (glx-instruction-jgeu nil (glx-32 -2) (glx-32 -1) glx-8)
    (should (equal *glx-pc* (glx-32 15)))
    (glx-instruction-jgeu nil (glx-32 -2) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 21)))
    (glx-instruction-jgeu nil (glx-32 1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 21)))))

(ert-deftest jgtu-instruction ()
  "jgtu instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-0))
    (glx-instruction-jgtu nil glx-2 glx-1 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jgtu nil glx-1 glx-2 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jgtu nil glx-1 glx-1 glx-8)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jgtu nil (glx-32 -1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jgtu nil (glx-32 -2) (glx-32 -1) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jgtu nil (glx-32 -2) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jgtu nil (glx-32 1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 9)))))

(ert-deftest jltu-instruction ()
  "jltu instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-0))
    (glx-instruction-jltu nil glx-2 glx-1 glx-5)
    (should (equal *glx-pc* glx-0))
    (glx-instruction-jltu nil glx-1 glx-2 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jltu nil glx-1 glx-1 glx-8)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jltu nil (glx-32 -1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jltu nil (glx-32 -2) (glx-32 -1) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jltu nil (glx-32 -2) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jltu nil (glx-32 -2) (glx-32 1) glx-8)
    (should (equal *glx-pc* (glx-32 9)))))

(ert-deftest jleu-instruction ()
  "jleu instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-0))
    (glx-instruction-jleu nil glx-2 glx-1 glx-5)
    (should (equal *glx-pc* glx-0))
    (glx-instruction-jleu nil glx-1 glx-2 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jleu nil glx-1 glx-1 glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jleu nil (glx-32 -1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jleu nil (glx-32 -2) (glx-32 -1) glx-8)
    (should (equal *glx-pc* (glx-32 15)))
    (glx-instruction-jleu nil (glx-32 -2) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 21)))
    (glx-instruction-jleu nil (glx-32 1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 27)))))

(ert-deftest neg-instruction ()
  "neg instruction"
  :tags '(instructions)

  ;; store to mem
  (with-glx-memory (0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 255 255 255 251 0 0 0 0)
    (glx-instruction-neg nil glx-5 (list #'glx-store-mem glx-4)))
  (with-glx-memory (0 0 0 0 255 255 255 251 0 0 0 0) (0 0 0 0 130 246 246 247 0 0 0 0)
    (glx-instruction-neg nil (glx-32 9 9 9 125) (list #'glx-store-mem glx-4))))

(ert-deftest stkcount-instruction ()
  "stkcount instruction"
  :tags '(instructions)

  ;; store to mem
  (with-glx-memory (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 2)
    (cl-letf (((symbol-function 'glx-stack-count) (lambda () glx-2)))
      (glx-instruction-stkcount nil (list #'glx-store-mem glx-4)))))

(ert-deftest sexb-instruction ()
  "sexb instruction"
  :tags '(instructions)

  (with-glx-memory (0 0 0 0 0 0 0 0) (0 0 0 0 255 255 255 150)
    (glx-instruction-sexb nil (glx-32 150 1 2 3) (list #'glx-store-mem glx-4)))

  (with-glx-memory (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 18)
    (glx-instruction-sexb nil (glx-32 18 1 2 3) (list #'glx-store-mem glx-4))))

(ert-deftest sexs-instruction ()
  "sexs instruction"
  :tags '(instructions)

  (with-glx-memory (0 0 0 0 0 0 0 0) (0 0 0 0 255 255 150 150)
    (glx-instruction-sexs nil (glx-32 150 150 2 3) (list #'glx-store-mem glx-4)))

  (with-glx-memory (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 18 18)
    (glx-instruction-sexs nil (glx-32 18 18 2 3) (list #'glx-store-mem glx-4))))

(ert-deftest catch-instruction ()
  "catch instruction"
  :tags '(instructions)

  ;; Store token to memory
  (with-glx-memory (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 1)
    (with-glx-stack () (1 4 3 0)
      (let ((*glx-pc* glx-3)
            (*glx-catch-token* glx-1))

        (glx-instruction-catch nil (list #'glx-store-mem glx-4) glx-5)
        (should (equal *glx-catch-token* glx-2))
        (should (equal *glx-pc* (glx-32 6))) ; pc + offset - 2
        ;; The top element of the call frame's stack. Can't see it by regular popping.
        (should (equal (caaar *glx-stack*) `(catch ,glx-1)))))

    ;; Store token onto stack
    (with-glx-stack () (3 0 3 0 17)
      (let ((*glx-pc* glx-3)
            (*glx-catch-token* (glx-32 17)))
        
        (glx-instruction-catch nil (list #'glx-store-stack nil) glx-5)
        ;; The second element of the call frame's stack. Can't see it by regular popping.
        (should (equal (cadaar *glx-stack*) `(catch ,(glx-32 17))))))))

(ert-deftest throw-instruction ()
  "throw instruction"
  :tags '(instructions)

  (with-glx-memory (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 5) ; The thrown value is stored according to the call stub on the stack
    (with-glx-stack (1 4 3 0) () ; Stack with a call stub as if created by catch
      (glx-value-push `(catch ,glx-1)) ; The catch token
      (glx-value-push glx-3) ; some other value to be discarded as we unwind.
      (glx-instruction-throw nil glx-5 glx-1))))

(ert-deftest save-should-write-game-state-to-buffer ()
  "save should write game state to buffer"
  :tags '(instructions)
  (with-glx-memory (0 0 0 0) (0 0 0 0)
    (let ((*glx-stack* (list (list nil (list (cons glx-0 glx-0)))))
          (*glx-pc* (list 0 3 4 5)))
      (with-temp-buffer
        (glx-save-game (current-buffer) 'dest-type 'dest-addr)
        (goto-char (point-min))
        (let ((saved-game (read (current-buffer))))
          (should (equal saved-game (list (make-vector 4 0)
                                          (list (list 'dest-type 'dest-addr (list 0 3 4 5)) (list nil (list (cons glx-0 glx-0)))))))))
      ;; call stub pushed during save should not be on the stack afterwards.
      (should (equal *glx-stack* (list (list nil (list (cons glx-0 glx-0)))))))))

(ert-deftest restore-game-should-load-from-buffer ()
  "restore game should load from buffer"
  :tags '(instructions)
  (unwind-protect
      (let ((*glx-memory* (make-vector 8 0)))
        (with-temp-buffer
          (insert "([0 1 2 3 0 0 0 0] ((1 (0 0 0 4) (7 8 9 10)) (nil (((0 0 0 0) 0 0 0 0)))))")
          (put 'stream 'buffer (current-buffer))
          (glx-restore-game 'stream)
          (should (equal *glx-memory* [0 1 2 3 255 255 255 255]))
          (should (equal *glx-stack* (list (list nil (list (cons glx-0 glx-0))))))))
    (put 'fileref 'buffer nil)))

(ert-deftest restore-game-should-load-from-buffer-and-not-overwrite-protected-memory ()
  "restore game should load from buffer and use protected memory range"
  :tags '(instructions)
  (unwind-protect
      (let ((*glx-protect* (cons 1 2))
            (*glx-memory* (vector 4 5 6 7 0 0 0 0)))
        (with-temp-buffer
          (insert "([0 1 2 3 0 0 0 0] ((1 (0 0 0 4) (7 8 9 10)) (nil (((0 0 0 0) 0 0 0 0)))))")
          (put 'stream 'buffer (current-buffer))
          (glx-restore-game 'stream)
          (should (equal *glx-memory* [0 5 6 3 255 255 255 255]))
          (should (equal *glx-stack* (list (list nil (list (cons glx-0 glx-0))))))))
    (put 'fileref 'buffer nil)))
