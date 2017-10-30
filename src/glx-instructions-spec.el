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

(ert-deftest call-instruction ()
  "call instruction"
  :tags '(instructions)
  (let ((call-args nil))

    (cl-flet ((test-glx-call-function (fptr dt da args) (setq call-args (list fptr dt da args))))

      ;; unwind-protect to override and reinstate (using advice) the glx-call-function definition
      (unwind-protect
          (progn
            (advice-add 'glx-call-function :override #'test-glx-call-function '((name . test-glx-call-function)))

            (glx-instruction-call glx-0 glx-0 (list #'glx-store-throw nil))
            (should (equal call-args (list glx-0 0 glx-0 nil)))

            ;; two args call - store result in memory
            (let ((*glx-stack* (list (list (list glx-2 glx-3)))))
              (glx-instruction-call glx-0 glx-2 (list #'glx-store-mem glx-4))
              (should (equal call-args (list glx-0 1 glx-4 (list glx-2 glx-3))))
              (should (equal *glx-stack* '((())))))

            ;; zero args call - push result
            (glx-instruction-call glx-0 glx-0 (list #'glx-store-stack nil))
            (should (equal call-args (list glx-0 3 glx-0 nil)))

            ;; zero args call - store result in a local
            (glx-instruction-call glx-0 glx-0 (list #'glx-store-local glx-5))
            (should (equal call-args (list glx-0 2 glx-5 nil))))

        (advice-remove 'glx-call-function 'test-glx-call-function)))))

(ert-deftest copy-instruction ()
  "copy instruction"
  :tags '(instructions)

  ;; copy to stack
  (let ((*glx-stack* (list (list nil))))
    (glx-instruction-copy glx-2 (list #'glx-store-stack nil))
    (should (equal *glx-stack* (list (list (list glx-2))))))

  ;; copy to local
  (let ((*glx-stack* (list (list nil (list (cons glx-2 glx-0))))))
    (glx-instruction-copy glx-2 (list #'glx-store-local glx-2))
    (should (equal *glx-stack* (list (list nil (list (cons glx-2 glx-2))))))))

(ert-deftest sub-instruction ()
  "sub instruction"
  :tags '(instructions)

  ;; store to mem
  (let ((*glx-memory* (make-vector 12 0)))
    (glx-instruction-sub glx-2 glx-1 (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [0 0 0 0 0 0 0 1 0 0 0 0]))))

(ert-deftest jlt-instruction ()
  "jlt instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-0))
    (glx-instruction-jlt glx-2 glx-1 glx-5)
    (should (equal *glx-pc* glx-0))
    (glx-instruction-jlt glx-1 glx-2 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jlt glx-1 glx-1 glx-8)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jlt (glx-32 -1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jlt (glx-32 -2) (glx-32 -1) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jlt (glx-32 -2) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jlt (glx-32 -2) (glx-32 1) glx-8)
    (should (equal *glx-pc* (glx-32 15))))

  (let (return-was-called)
    (cl-flet ((test-glx-instruction-return (result) (setq return-was-called result)))
      (unwind-protect
          (progn
            (advice-add 'glx-instruction-return :override #'test-glx-instruction-return '((name . test-glx-instruction-return)))
            
            (glx-instruction-jlt glx-1 glx-2 glx-1)
            (should (equal return-was-called glx-1)))
        (advice-remove 'glx-instruction-return 'test-glx-instruction-return)))))

(ert-deftest getmemsize-instruction ()
  "getmemsize instruction"
  :tags '(instructions)

  (let ((*glx-memory* (make-vector 8 0)))
    (glx-instruction-getmemsize (list #'glx-store-mem glx-1))
    (should (equal *glx-memory* [0 0 0 0 8 0 0 0]))))

(ert-deftest jne-instruction ()
  "jne instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-4))
    (glx-instruction-jne glx-2 glx-1 glx-5)
    (should (equal *glx-pc* (glx-32 7)))
    (glx-instruction-jne glx-1 glx-1 glx-5)
    (should (equal *glx-pc* (glx-32 7))))

  (let (return-was-called)
    (cl-flet ((test-glx-instruction-return (result) (setq return-was-called result)))
      (unwind-protect
          (progn
            (advice-add 'glx-instruction-return :override #'test-glx-instruction-return '((name . test-glx-instruction-return)))

            (glx-instruction-jne glx-1 glx-2 glx-0)
            (should (equal return-was-called glx-0)))
        (advice-remove 'glx-instruction-return 'test-glx-instruction-return)))))

(ert-deftest return-instruction ()
  "return instruction"
  :tags '(instructions)

  (let ((call-count 0))
    (cl-flet ((test-glx-return-from-function () (cond ((= call-count 0) (incf call-count) (list 0 glx-0 glx-5))
                                                      ((= call-count 1) (incf call-count) (list 3 glx-0 glx-5))
                                                      ((= call-count 2) (incf call-count) (list 2 glx-2 glx-5))
                                                      ((= call-count 3) (incf call-count) (list 1 glx-2 glx-5)))))
      
      (unwind-protect
          (progn
            (advice-add 'glx-return-from-function :override #'test-glx-return-from-function '((name . test-glx-return-from-function)))
            ;; ignore result
            (glx-instruction-return glx-3)
            (should (= call-count 1))

            ;; push result onto stack
            (let ((*glx-stack* (list (list nil))))
              (glx-instruction-return glx-4)
              (should (equal *glx-stack* (list (list (list glx-4)))))
              (should (= call-count 2)))

            ;; store result in local
            (let ((*glx-stack* (list (list nil (list (cons glx-2 glx-0))))))
              (glx-instruction-return glx-4)
              (should (equal *glx-stack* (list (list nil (list (cons glx-2 glx-4))))))
              (should (= call-count 3)))

            ;; store result in memory
            (let ((*glx-memory* (make-vector 8 0)))
              (glx-instruction-return (glx-32 4 5 6 7))
              (should (equal *glx-memory* [0 0 7 6 5 4 0 0]))
              (should (= call-count 4))))
        
        (advice-remove 'glx-return-from-function 'test-glx-return-from-function)))))

(ert-deftest jge-instruction ()
  "jge instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-0))
    (glx-instruction-jge glx-2 glx-1 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jge glx-1 glx-2 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jge glx-1 glx-1 glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jge (glx-32 -1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 15)))
    (glx-instruction-jge (glx-32 -2) (glx-32 -1) glx-8)
    (should (equal *glx-pc* (glx-32 15)))
    (glx-instruction-jge (glx-32 -2) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 21)))
    (glx-instruction-jge (glx-32 1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 27)))))

(ert-deftest jle-instruction ()
  "jle instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-0))
    (glx-instruction-jle glx-2 glx-1 glx-5)
    (should (equal *glx-pc* glx-0))
    (glx-instruction-jle glx-1 glx-2 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jle glx-1 glx-1 glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jle (glx-32 -1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jle (glx-32 -2) (glx-32 -1) glx-8)
    (should (equal *glx-pc* (glx-32 15)))
    (glx-instruction-jle (glx-32 -2) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 21)))
    (glx-instruction-jle (glx-32 1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 21)))))

(ert-deftest aloadb-instruction ()
  "aloadb instruction"
  :tags '(instructions)

  (let ((*glx-memory* (vector 0 1 2 3 4 5 6 7 8)))
    (glx-instruction-aloadb glx-2 glx-1 (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [0 1 2 3 0 0 0 3 8]))))

(ert-deftest jgt-instruction ()
  "jgt instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-0))
    (glx-instruction-jgt glx-2 glx-1 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jgt glx-1 glx-2 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jgt glx-1 glx-1 glx-8)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jgt (glx-32 -1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jgt (glx-32 -2) (glx-32 -1) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jgt (glx-32 -2) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jgt (glx-32 1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 15)))))

(ert-deftest aload-instruction ()
  "aload instruction"
  :tags '(instructions)

  (let ((*glx-memory* [0 1 2 3 4 5 6 7 8 9 10])
        (*glx-stack* (list (list nil))))
    (glx-instruction-aload glx-2 glx-1 (list #'glx-store-stack nil))
    (should (equal *glx-stack* (list (list (list (glx-32 9 8 7 6))))))))

(ert-deftest jeq-instruction ()
  "jeq instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-0))
    (glx-instruction-jeq glx-2 glx-1 glx-5)
    (should (equal *glx-pc* glx-0))
    (glx-instruction-jeq glx-1 glx-2 glx-5)
    (should (equal *glx-pc* glx-0))
    (glx-instruction-jeq glx-1 glx-1 glx-8)
    (should (equal *glx-pc* (glx-32 6)))
    (glx-instruction-jeq (glx-32 -1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 6)))
    (glx-instruction-jeq (glx-32 -2) (glx-32 -1) glx-8)
    (should (equal *glx-pc* (glx-32 6)))
    (glx-instruction-jeq (glx-32 -2) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 12)))))

(ert-deftest jz-instruction ()
  "jz instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-0))
    (glx-instruction-jz glx-0 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jz glx-1 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jz (glx-32 -1) glx-8)
    (should (equal *glx-pc* glx-3))))

(ert-deftest jnz-instruction ()
  "jnz instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-0))
    (glx-instruction-jnz glx-0 glx-5)
    (should (equal *glx-pc* glx-0))
    (glx-instruction-jnz glx-1 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jnz (glx-32 -1) glx-8)
    (should (equal *glx-pc* (glx-32 9)))))

(ert-deftest add-instruction ()
  "add instruction"
  :tags '(instructions)

  ;; store to mem
  (let ((*glx-memory* (make-vector 12 0)))
    (glx-instruction-add glx-5 glx-8 (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [0 0 0 0 0 0 0 13 0 0 0 0]))))

(ert-deftest aloadbit-instruction ()
  "aloadbit instruction"
  :tags '(instructions)

  ;; store to mem
  (let ((*glx-memory* (vector 130 1 0 0 255 255 255 255)))
    (glx-instruction-aloadbit glx-0 glx-0 (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [130 1 0 0 0 0 0 0]))
    (glx-instruction-aloadbit glx-0 glx-1 (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [130 1 0 0 0 0 0 1]))
    (setq *glx-memory* [130 1 0 0 255 255 255 255])
    (glx-instruction-aloadbit glx-0 (glx-32 7) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [130 1 0 0 0 0 0 1]))
    (setq *glx-memory* [130 1 0 0 255 255 255 255])
    (glx-instruction-aloadbit glx-0 glx-8 (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [130 1 0 0 0 0 0 1]))
    (glx-instruction-aloadbit glx-0 (glx-32 9) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [130 1 0 0 0 0 0 0]))
    (glx-instruction-aloadbit glx-1 (glx-32 -1) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [130 1 0 0 0 0 0 1]))
    (glx-instruction-aloadbit glx-1 (glx-32 -2) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [130 1 0 0 0 0 0 0]))
    (glx-instruction-aloadbit glx-1 (glx-32 -7) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [130 1 0 0 0 0 0 1]))
    (setq *glx-memory* [130 1 0 0 255 255 255 255])
    (glx-instruction-aloadbit glx-1 (glx-32 -8) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [130 1 0 0 0 0 0 0]))
    (glx-instruction-aloadbit glx-2 (glx-32 -9) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [130 1 0 0 0 0 0 1])))
  (let ((*glx-memory* (vector 0 0 32 0 0 0 0 0)))
    (glx-instruction-aloadbit glx-0 (glx-32 21) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [0 0 32 0 0 0 0 1]))
    (setq *glx-memory* [0 0 32 0 255 255 255 255])
    (glx-instruction-aloadbit (glx-32 6) (glx-32 -27) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [0 0 32 0 0 0 0 1]))))

(ert-deftest aloads-instruction ()
  "aloads instruction"
  :tags '(instructions)
  (let ((*glx-memory* [1 2 3 4 5 6 7 8 9])
        (*glx-stack* (list (list nil))))
    (glx-instruction-aloads glx-0 glx-0 (list #'glx-store-stack nil))
    (should (equal *glx-stack* `(((,(glx-32 2 1))))))
    (setq *glx-stack* (list (list nil)))
    (glx-instruction-aloads glx-0 glx-3 (list #'glx-store-stack nil))
    (should (equal *glx-stack* `(((,(glx-32 8 7))))))
    (setq *glx-stack* (list (list nil)))
    (glx-instruction-aloads glx-4 glx-1 (list #'glx-store-stack nil))
    (should (equal *glx-stack* `(((,(glx-32 8 7))))))))

(ert-deftest mul-instruction ()
  "mul instruction"
  :tags '(instructions)

  ;; store to mem
  (let ((*glx-memory* (make-vector 12 0)))
    (glx-instruction-mul glx-5 glx-8 (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [0 0 0 0 0 0 0 40 0 0 0 0]))))

(ert-deftest stkcopy-instruction ()
  "stkcopy instruction"
  :tags '(instructions)

  (let ((*glx-stack* (list (list (list glx-0 glx-1 glx-2 glx-3)))))
    (glx-instruction-stkcopy glx-3)
    (should (equal *glx-stack* (list (list (list glx-0 glx-1 glx-2 glx-0 glx-1 glx-2 glx-3)))))))

(ert-deftest gestalt-instruction ()
  "gestalt instruction"
  :tags '(instructions)

  (let ((*glx-memory* (make-vector 5 0)))
    (glx-instruction-gestalt glx-0 glx-0 (list #'glx-store-mem glx-1))
    (should (equal *glx-memory* [0 0 3 1 0]))
    (glx-instruction-gestalt glx-1 glx-0 (list #'glx-store-mem glx-1))
    (should (equal *glx-memory* [0 0 0 0 0]))
    (glx-instruction-gestalt glx-2 glx-0 (list #'glx-store-mem glx-1))
    (should (equal *glx-memory* [0 0 0 0 1]))
    (glx-instruction-gestalt glx-3 glx-0 (list #'glx-store-mem glx-1))
    (should (equal *glx-memory* [0 0 0 0 0]))
    (glx-instruction-gestalt glx-4 glx-0 (list #'glx-store-mem glx-1))
    (should (equal *glx-memory* [0 0 0 0 1]))
    (glx-instruction-gestalt glx-4 glx-2 (list #'glx-store-mem glx-1))
    (should (equal *glx-memory* [0 0 0 0 1]))
    (glx-instruction-gestalt glx-4 glx-2 (list #'glx-store-mem glx-1))
    (should (equal *glx-memory* [0 0 0 0 1]))
    (glx-instruction-gestalt glx-4 glx-3 (list #'glx-store-mem glx-1))
    (should (equal *glx-memory* [0 0 0 0 0]))))

(ert-deftest setiosys ()
  "setiosys"
  :tags '(instructions)
  (let ((*glx-glk-selected* nil))
    (glx-instruction-setiosys glx-2 glx-0)
    (should *glx-glk-selected*)
    (should-error (glx-instruction-setiosys glx-3 glx-0) :type 'glx-glk-error)))

(ert-deftest jump-instruction ()
  "jump instruction"
  :tags '(instructions)
  (let ((*glx-pc* glx-0))
    (glx-instruction-jump glx-5)
    (should (equal *glx-pc* glx-3))))

(ert-deftest astore-instruction ()
  "astore instruction"
  :tags '(instructions)
  (let ((*glx-memory* (make-vector 16 0)))
    (glx-instruction-astore glx-4 glx-2 glx-8)
    (should (equal *glx-memory* [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8]))))

(ert-deftest copys-instruction ()
  "copys instruction"
  :tags '(instructions)

  ;; copy to stack
  (let ((*glx-stack* (list (list nil))))
    (glx-instruction-copys (glx-32 7 6 5 4) (list #'glx-store-stack nil))
    (should (equal *glx-stack* (list (list (list (glx-32 5 4)))))))

  ;; copy to local
  (let ((*glx-stack* (list (list nil (list (cons glx-2 glx-0))))))
    (glx-instruction-copys (glx-32 9 8 7 6) (list #'glx-store-local glx-2))
    (should (equal *glx-stack* (list (list nil (list (cons glx-2 (glx-32 7 6))))))))

  ;; copy to memory
  (let ((*glx-memory* [0 0 0 0]))
    (glx-instruction-copys (glx-32 9 8 7 6) (list #'glx-store-mem glx-2))
    (should (equal *glx-memory* [0 0 6 7]))))

(ert-deftest copyb-instruction ()
  "copyb instruction"
  :tags '(instructions)

  ;; copy to stack
  (let ((*glx-stack* (list (list nil))))
    (glx-instruction-copyb (glx-32 7 6 5 4) (list #'glx-store-stack nil))
    (should (equal *glx-stack* (list (list (list (glx-32 4)))))))

  ;; copy to local
  (let ((*glx-stack* (list (list nil (list (cons glx-2 glx-0))))))
    (glx-instruction-copyb (glx-32 9 8 7 6) (list #'glx-store-local glx-2))
    (should (equal *glx-stack* (list (list nil (list (cons glx-2 (glx-32 6))))))))

  ;; copy to memory
  (let ((*glx-memory* [0 0 0 0]))
    (glx-instruction-copyb (glx-32 9 8 7 6) (list #'glx-store-mem glx-2))
    (should (equal *glx-memory* [0 0 6 0]))))

(ert-deftest astorebit-instruction ()
  "astorebit instruction"
  :tags '(instructions)

  ;; store to mem
  (let ((*glx-memory* (vector 130 1 0 0)))
    (glx-instruction-astorebit glx-0 glx-0 glx-1)
    (should (equal *glx-memory* [131 1 0 0]))
    (glx-instruction-astorebit glx-0 glx-0 glx-0)
    (should (equal *glx-memory* [130 1 0 0]))
    (glx-instruction-astorebit glx-0 (glx-32 7) glx-0)
    (should (equal *glx-memory* [2 1 0 0]))
    (glx-instruction-astorebit glx-0 (glx-32 7) glx-1)
    (should (equal *glx-memory* [130 1 0 0]))
    (glx-instruction-astorebit glx-0 (glx-32 9) glx-0)
    (should (equal *glx-memory* [130 1 0 0]))
    (glx-instruction-astorebit glx-1 (glx-32 -1) glx-0)
    (should (equal *glx-memory* [2 1 0 0]))
    (glx-instruction-astorebit glx-1 (glx-32 -2) glx-1)
    (should (equal *glx-memory* [66 1 0 0]))
    (glx-instruction-astorebit glx-1 (glx-32 -7) glx-0)
    (should (equal *glx-memory* [64 1 0 0]))
    (glx-instruction-astorebit glx-1 (glx-32 -8) glx-1)
    (should (equal *glx-memory* [65 1 0 0]))
    (glx-instruction-astorebit glx-2 (glx-32 -9) glx-1)
    (should (equal *glx-memory* [193 1 0 0]))
    (glx-instruction-astorebit glx-0 (glx-32 21) glx-1)
    (should (equal *glx-memory* [193 1 32 0]))))

(ert-deftest astoreb-instruction ()
  "astoreb instruction"
  :tags '(instructions)
  (let ((*glx-memory* (vector 0 1 2 3 4 5 6 7 8)))
    (glx-instruction-astoreb glx-1 glx-2 (glx-32 4 5 6 7))
    (should (equal *glx-memory* [0 1 2 4 4 5 6 7 8]))))

(ert-deftest callf-instruction ()
  "callf instruction"
  :tags '(instructions)

  (let ((call-args nil))
    (cl-flet ((test-glx-call-function (fptr dt da args) (setq call-args (list fptr dt da args))))

        (unwind-protect
          (progn
            (advice-add 'glx-call-function :override #'test-glx-call-function '((name . test-glx-call-function)))

            ;; store result in memory
            (glx-instruction-callf glx-1 (list #'glx-store-mem glx-4))
            (should (equal call-args (list glx-1 1 glx-4 nil))))

          (advice-remove 'glx-call-function 'test-glx-call-function)))))

(ert-deftest callfi-instruction ()
  "callfi instruction"
  :tags '(instructions)
  (let ((call-args nil))
    (cl-flet ((test-glx-call-function (fptr dt da args) (setq call-args (list fptr dt da args))))

      (unwind-protect
          (progn
            (advice-add 'glx-call-function :override #'test-glx-call-function '((name . test-glx-call-function)))
            
            ;; store result in memory
            (glx-instruction-callfi glx-1 glx-4 (list #'glx-store-mem glx-3))
            (should (equal call-args (list glx-1 1 glx-3 (list glx-4)))))
        
        (advice-remove 'glx-call-function 'test-glx-call-function)))))

(ert-deftest callfii-instruction ()
  "callfii instruction"
  :tags '(instructions)
  (let ((call-args nil))
    (cl-flet ((test-glx-call-function (fptr dt da args) (setq call-args (list fptr dt da args))))

      (unwind-protect
          (progn
            (advice-add 'glx-call-function :override #'test-glx-call-function '((name . test-glx-call-function)))
            
            ;; store result in memory
            (glx-instruction-callfii glx-1 glx-2 glx-3 (list #'glx-store-mem glx-8))
            (should (equal call-args (list glx-1 1 glx-8 (list glx-2 glx-3)))))
        
        (advice-remove 'glx-call-function 'test-glx-call-function)))))

(ert-deftest callfiii-instruction ()
  "callfiii instruction"
  :tags '(instructions)
  (let ((call-args nil))
    (cl-flet ((test-glx-call-function (fptr dt da args) (setq call-args (list fptr dt da args))))

      (unwind-protect
          (progn
            (advice-add 'glx-call-function :override #'test-glx-call-function '((name . test-glx-call-function)))
            
            ;; store result in memory
            (glx-instruction-callfiii glx-0 glx-5 glx-4 glx-3 (list #'glx-store-mem glx-4))
            (should (equal call-args (list glx-0 1 glx-4 (list glx-5 glx-4 glx-3)))))
        
        (advice-remove 'glx-call-function 'test-glx-call-function)))))

(ert-deftest jgeu-instruction ()
  "jgeu instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-0))
    (glx-instruction-jgeu glx-2 glx-1 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jgeu glx-1 glx-2 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jgeu glx-1 glx-1 glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jgeu (glx-32 -1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 15)))
    (glx-instruction-jgeu (glx-32 -2) (glx-32 -1) glx-8)
    (should (equal *glx-pc* (glx-32 15)))
    (glx-instruction-jgeu (glx-32 -2) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 21)))
    (glx-instruction-jgeu (glx-32 1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 21)))))

(ert-deftest jgtu-instruction ()
  "jgtu instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-0))
    (glx-instruction-jgtu glx-2 glx-1 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jgtu glx-1 glx-2 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jgtu glx-1 glx-1 glx-8)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jgtu (glx-32 -1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jgtu (glx-32 -2) (glx-32 -1) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jgtu (glx-32 -2) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jgtu (glx-32 1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 9)))))

(ert-deftest jltu-instruction ()
  "jltu instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-0))
    (glx-instruction-jltu glx-2 glx-1 glx-5)
    (should (equal *glx-pc* glx-0))
    (glx-instruction-jltu glx-1 glx-2 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jltu glx-1 glx-1 glx-8)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jltu (glx-32 -1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jltu (glx-32 -2) (glx-32 -1) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jltu (glx-32 -2) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jltu (glx-32 -2) (glx-32 1) glx-8)
    (should (equal *glx-pc* (glx-32 9)))))

(ert-deftest jleu-instruction ()
  "jleu instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-0))
    (glx-instruction-jleu glx-2 glx-1 glx-5)
    (should (equal *glx-pc* glx-0))
    (glx-instruction-jleu glx-1 glx-2 glx-5)
    (should (equal *glx-pc* glx-3))
    (glx-instruction-jleu glx-1 glx-1 glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jleu (glx-32 -1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 9)))
    (glx-instruction-jleu (glx-32 -2) (glx-32 -1) glx-8)
    (should (equal *glx-pc* (glx-32 15)))
    (glx-instruction-jleu (glx-32 -2) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 21)))
    (glx-instruction-jleu (glx-32 1) (glx-32 -2) glx-8)
    (should (equal *glx-pc* (glx-32 27)))))

(ert-deftest neg-instruction ()
  "neg instruction"
  :tags '(instructions)

  ;; store to mem
  (let ((*glx-memory* (make-vector 12 0)))
    (glx-instruction-neg glx-5 (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [0 0 0 0 255 255 255 251 0 0 0 0]))
    (glx-instruction-neg (glx-32 9 9 9 125) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [0 0 0 0 130 246 246 247 0 0 0 0]))))
