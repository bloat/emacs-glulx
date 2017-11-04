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

    (cl-letf (((symbol-function 'glx-call-function) (lambda (fptr dt da args) (setq call-args (list fptr dt da args)))))

      (glx-instruction-call nil glx-0 glx-0 (list #'glx-store-throw nil))
      (should (equal call-args (list glx-0 0 glx-0 nil)))

      ;; two args call - store result in memory
      (let ((*glx-stack* `(((,glx-2 ,glx-3) ()))))
        (glx-instruction-call nil glx-0 glx-2 (list #'glx-store-mem glx-4))
        (should (equal call-args (list glx-0 1 glx-4 (list glx-2 glx-3))))
        (should (equal *glx-stack* '((() ())))))

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
      (let ((*glx-stack* `(((,glx-2 ,glx-3) ()))))
        (glx-instruction-tailcall nil glx-0 glx-2)
        (should (equal call-args (list glx-0 (list glx-2 glx-3))))
        (should (equal *glx-stack* '((() ())))))

      ;; zero args call
      (glx-instruction-tailcall nil glx-0 glx-0)
      (should (equal call-args (list glx-0 nil))))))

(ert-deftest copy-instruction ()
  "copy instruction"
  :tags '(instructions)

  ;; copy to stack
  (let ((*glx-stack* (list (list nil))))
    (glx-instruction-copy nil glx-2 (list #'glx-store-stack nil))
    (should (equal *glx-stack* (list (list (list glx-2))))))

  ;; copy to local
  (let ((*glx-stack* (list (list nil (list (cons glx-2 glx-0))))))
    (glx-instruction-copy nil glx-2 (list #'glx-store-local glx-2))
    (should (equal *glx-stack* (list (list nil (list (cons glx-2 glx-2))))))))

(ert-deftest sub-instruction ()
  "sub instruction"
  :tags '(instructions)

  ;; store to mem
  (let ((*glx-memory* (make-vector 12 0)))
    (glx-instruction-sub nil glx-2 glx-1 (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [0 0 0 0 0 0 0 1 0 0 0 0]))))

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
    (should (equal *glx-pc* (glx-32 15))))

  (let (return-was-called)
    (cl-flet ((test-glx-instruction-return (modes result) (setq return-was-called result)))
      (unwind-protect
          (progn
            (advice-add 'glx-instruction-return :override #'test-glx-instruction-return '((name . test-glx-instruction-return)))
            
            (glx-instruction-jlt nil glx-1 glx-2 glx-1)
            (should (equal return-was-called glx-1)))
        (advice-remove 'glx-instruction-return 'test-glx-instruction-return)))))

(ert-deftest getmemsize-instruction ()
  "getmemsize instruction"
  :tags '(instructions)

  (let ((*glx-memory* (make-vector 8 0)))
    (glx-instruction-getmemsize nil (list #'glx-store-mem glx-1))
    (should (equal *glx-memory* [0 0 0 0 8 0 0 0]))))

(ert-deftest jne-instruction ()
  "jne instruction"
  :tags '(instructions)

  (let ((*glx-pc* glx-4))
    (glx-instruction-jne nil glx-2 glx-1 glx-5)
    (should (equal *glx-pc* (glx-32 7)))
    (glx-instruction-jne nil glx-1 glx-1 glx-5)
    (should (equal *glx-pc* (glx-32 7))))

  (let (return-was-called)
    (cl-flet ((test-glx-instruction-return (modes result) (setq return-was-called result)))
      (unwind-protect
          (progn
            (advice-add 'glx-instruction-return :override #'test-glx-instruction-return '((name . test-glx-instruction-return)))

            (glx-instruction-jne nil glx-1 glx-2 glx-0)
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
            (glx-instruction-return nil glx-3)
            (should (= call-count 1))

            ;; push result onto stack
            (let ((*glx-stack* (list (list nil))))
              (glx-instruction-return nil glx-4)
              (should (equal *glx-stack* (list (list (list glx-4)))))
              (should (= call-count 2)))

            ;; store result in local
            (let ((*glx-stack* (list (list nil (list (cons glx-2 glx-0))))))
              (glx-instruction-return nil glx-4)
              (should (equal *glx-stack* (list (list nil (list (cons glx-2 glx-4))))))
              (should (= call-count 3)))

            ;; store result in memory
            (let ((*glx-memory* (make-vector 8 0)))
              (glx-instruction-return nil (glx-32 4 5 6 7))
              (should (equal *glx-memory* [0 0 7 6 5 4 0 0]))
              (should (= call-count 4))))
        
        (advice-remove 'glx-return-from-function 'test-glx-return-from-function)))))

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
    (should (equal *glx-pc* (glx-32 27)))))

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
    (should (equal *glx-pc* (glx-32 21)))))

(ert-deftest aloadb-instruction ()
  "aloadb instruction"
  :tags '(instructions)

  (let ((*glx-memory* (vector 0 1 2 3 4 5 6 7 8)))
    (glx-instruction-aloadb nil glx-2 glx-1 (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [0 1 2 3 0 0 0 3 8]))))

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
    (should (equal *glx-pc* (glx-32 15)))))

(ert-deftest aload-instruction ()
  "aload instruction"
  :tags '(instructions)

  (let ((*glx-memory* [0 1 2 3 4 5 6 7 8 9 10])
        (*glx-stack* '((() ()))))
    (glx-instruction-aload nil glx-2 glx-1 (list #'glx-store-stack nil))
    (should (equal *glx-stack* `(((,(glx-32 9 8 7 6)) ()))))))

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

(ert-deftest add-instruction ()
  "add instruction"
  :tags '(instructions)

  ;; store to mem
  (let ((*glx-memory* (make-vector 12 0)))
    (glx-instruction-add nil glx-5 glx-8 (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [0 0 0 0 0 0 0 13 0 0 0 0]))))

(ert-deftest aloadbit-instruction ()
  "aloadbit instruction"
  :tags '(instructions)

  ;; store to mem
  (let ((*glx-memory* (vector 130 1 0 0 255 255 255 255)))
    (glx-instruction-aloadbit nil glx-0 glx-0 (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [130 1 0 0 0 0 0 0]))
    (glx-instruction-aloadbit nil glx-0 glx-1 (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [130 1 0 0 0 0 0 1]))
    (setq *glx-memory* [130 1 0 0 255 255 255 255])
    (glx-instruction-aloadbit nil glx-0 (glx-32 7) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [130 1 0 0 0 0 0 1]))
    (setq *glx-memory* [130 1 0 0 255 255 255 255])
    (glx-instruction-aloadbit nil glx-0 glx-8 (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [130 1 0 0 0 0 0 1]))
    (glx-instruction-aloadbit nil glx-0 (glx-32 9) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [130 1 0 0 0 0 0 0]))
    (glx-instruction-aloadbit nil glx-1 (glx-32 -1) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [130 1 0 0 0 0 0 1]))
    (glx-instruction-aloadbit nil glx-1 (glx-32 -2) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [130 1 0 0 0 0 0 0]))
    (glx-instruction-aloadbit nil glx-1 (glx-32 -7) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [130 1 0 0 0 0 0 1]))
    (setq *glx-memory* [130 1 0 0 255 255 255 255])
    (glx-instruction-aloadbit nil glx-1 (glx-32 -8) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [130 1 0 0 0 0 0 0]))
    (glx-instruction-aloadbit nil glx-2 (glx-32 -9) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [130 1 0 0 0 0 0 1])))
  (let ((*glx-memory* (vector 0 0 32 0 0 0 0 0)))
    (glx-instruction-aloadbit nil glx-0 (glx-32 21) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [0 0 32 0 0 0 0 1]))
    (setq *glx-memory* [0 0 32 0 255 255 255 255])
    (glx-instruction-aloadbit nil (glx-32 6) (glx-32 -27) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [0 0 32 0 0 0 0 1]))))

(ert-deftest aloads-instruction ()
  "aloads instruction"
  :tags '(instructions)
  (let ((*glx-memory* [1 2 3 4 5 6 7 8 9])
        (*glx-stack* (list (list nil))))
    (glx-instruction-aloads nil glx-0 glx-0 (list #'glx-store-stack nil))
    (should (equal *glx-stack* `(((,(glx-32 2 1))))))
    (setq *glx-stack* (list (list nil)))
    (glx-instruction-aloads nil glx-0 glx-3 (list #'glx-store-stack nil))
    (should (equal *glx-stack* `(((,(glx-32 8 7))))))
    (setq *glx-stack* (list (list nil)))
    (glx-instruction-aloads nil glx-4 glx-1 (list #'glx-store-stack nil))
    (should (equal *glx-stack* `(((,(glx-32 8 7))))))))

(ert-deftest mul-instruction ()
  "mul instruction"
  :tags '(instructions)

  ;; store to mem
  (let ((*glx-memory* (make-vector 12 0)))
    (glx-instruction-mul nil glx-5 glx-8 (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [0 0 0 0 0 0 0 40 0 0 0 0]))))

(ert-deftest stkcopy-instruction ()
  "stkcopy instruction"
  :tags '(instructions)

  (let ((*glx-stack* (list (list (list glx-0 glx-1 glx-2 glx-3)))))
    (glx-instruction-stkcopy nil glx-3)
    (should (equal *glx-stack* (list (list (list glx-0 glx-1 glx-2 glx-0 glx-1 glx-2 glx-3)))))))

(ert-deftest gestalt-instruction ()
  "gestalt instruction"
  :tags '(instructions)

  (let ((*glx-memory* (make-vector 5 0)))
    (glx-instruction-gestalt nil glx-0 glx-0 (list #'glx-store-mem glx-1))
    (should (equal *glx-memory* [0 0 3 1 0]))
    (glx-instruction-gestalt nil glx-1 glx-0 (list #'glx-store-mem glx-1))
    (should (equal *glx-memory* [0 0 0 0 0]))
    (glx-instruction-gestalt nil glx-2 glx-0 (list #'glx-store-mem glx-1))
    (should (equal *glx-memory* [0 0 0 0 1]))
    (glx-instruction-gestalt nil glx-3 glx-0 (list #'glx-store-mem glx-1))
    (should (equal *glx-memory* [0 0 0 0 0]))
    (glx-instruction-gestalt nil glx-4 glx-0 (list #'glx-store-mem glx-1))
    (should (equal *glx-memory* [0 0 0 0 1]))
    (glx-instruction-gestalt nil glx-4 glx-2 (list #'glx-store-mem glx-1))
    (should (equal *glx-memory* [0 0 0 0 1]))
    (glx-instruction-gestalt nil glx-4 glx-2 (list #'glx-store-mem glx-1))
    (should (equal *glx-memory* [0 0 0 0 1]))
    (glx-instruction-gestalt nil glx-4 glx-3 (list #'glx-store-mem glx-1))
    (should (equal *glx-memory* [0 0 0 0 0]))))

(ert-deftest setiosys ()
  "setiosys"
  :tags '(instructions)
  (let ((*glx-glk-selected* nil))
    (glx-instruction-setiosys nil glx-2 glx-0)
    (should *glx-glk-selected*)
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
  (let ((*glx-memory* (make-vector 16 0)))
    (glx-instruction-astore nil glx-4 glx-2 glx-8)
    (should (equal *glx-memory* [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8]))))

(ert-deftest astores-instruction ()
  "astores instruction"
  :tags '(instructions)
  (let ((*glx-memory* (make-vector 10 0)))
    (glx-instruction-astores nil glx-4 glx-2 glx-8)
    (should (equal *glx-memory* [0 0 0 0 0 0 0 0 0 8]))))

(ert-deftest copys-instruction ()
  "copys instruction"
  :tags '(instructions)

  ;; copy to stack - constant - from a 4 byte constant copys takes the lowest 2 bytes
  (let ((*glx-stack* (list (list nil))))
    (glx-instruction-copys '(3 8) (glx-32 7 6 5 4) (list #'glx-store-stack nil))
    (should (equal *glx-stack* `(((,(glx-32 7 6)))))))

  ;; copy to memory - constant - from a 4 byte constant copys takes the lowest 2 bytes
  (let ((*glx-memory* [0 0 0 0]))
    (glx-instruction-copys '(3 7) (glx-32 9 8 7 6) (list #'glx-store-mem glx-2))
    (should (equal *glx-memory* [0 0 8 9])))

  ;; copy to memory - 2 bytes popped from stack - the system pops a 4 byte value, copys
  ;; takes the lowest 2 bytes.
  (let ((*glx-memory* [0 0 0 0]))
    (glx-instruction-copys '(8 7) (glx-32 9 8 7 6) (list #'glx-store-mem glx-2))
    (should (equal *glx-memory* [0 0 8 9])))

  ;; copy to stack - 2 bytes loaded from memory - the system provides 4 bytes, but we want
  ;; the 2 bytes at the given memory address, i.e. the top two bytes 
  (let ((*glx-stack* (list (list nil))))
    (glx-instruction-copys '(#xe 8) (glx-32 7 6 5 4) (list #'glx-store-stack nil))
    (should (equal *glx-stack* `(((,(glx-32 5 4)))))))

  ;; copy to memory - 2 bytes loaded from memory - the system provides 4 bytes, but we want
  ;; the 2 bytes at the given memory address, i.e. the top two bytes 
  (let ((*glx-memory* [0 0 0 0]))
    (glx-instruction-copys '(7 7) (glx-32 9 8 7 6) (list #'glx-store-mem glx-2))
    (should (equal *glx-memory* [0 0 6 7]))))

(ert-deftest copyb-instruction ()
  "copyb instruction"
  :tags '(instructions)

  ;; copy to stack - constant - from a 4 byte constant copyb takes the lowest byte
  (let ((*glx-stack* (list (list nil))))
    (glx-instruction-copyb '(3 8) (glx-32 7 6 5 4) (list #'glx-store-stack nil))
    (should (equal *glx-stack* `(((,(glx-32 7)))))))

  ;; copy to memory - constant - from a 4 byte constant copyb takes the lowest byte
  (let ((*glx-memory* [0 0 0 0]))
    (glx-instruction-copyb '(3 7) (glx-32 9 8 7 6) (list #'glx-store-mem glx-2))
    (should (equal *glx-memory* [0 0 9 0])))

  ;; copy to memory - 1 byte popped from stack - the system pops a 4 byte value, copyb
  ;; takes the lowest byte.
  (let ((*glx-memory* [0 0 0 0]))
    (glx-instruction-copyb '(8 7) (glx-32 9 8 7 6) (list #'glx-store-mem glx-2))
    (should (equal *glx-memory* [0 0 9 0])))

  ;; copy to stack - 1 byte loaded from memory - the system provides 4 bytes, but we want
  ;; the byte at the given memory address, i.e. the top byte
  (let ((*glx-stack* (list (list nil))))
    (glx-instruction-copyb '(#xe 8) (glx-32 7 6 5 4) (list #'glx-store-stack nil))
    (should (equal *glx-stack* `(((,(glx-32 4)))))))

  ;; copy to memory - 1 byte loaded from memory - the system provides 4 bytes, but we want
  ;; the byte at the given memory address, i.e. the top byte
  (let ((*glx-memory* [0 0 0 0]))
    (glx-instruction-copyb '(7 7) (glx-32 9 8 7 6) (list #'glx-store-mem glx-2))
    (should (equal *glx-memory* [0 0 6 0]))))

(ert-deftest astorebit-instruction ()
  "astorebit instruction"
  :tags '(instructions)

  ;; store to mem
  (let ((*glx-memory* (vector 130 1 0 0)))
    (glx-instruction-astorebit nil glx-0 glx-0 glx-1)
    (should (equal *glx-memory* [131 1 0 0]))
    (glx-instruction-astorebit nil glx-0 glx-0 glx-0)
    (should (equal *glx-memory* [130 1 0 0]))
    (glx-instruction-astorebit nil glx-0 (glx-32 7) glx-0)
    (should (equal *glx-memory* [2 1 0 0]))
    (glx-instruction-astorebit nil glx-0 (glx-32 7) glx-1)
    (should (equal *glx-memory* [130 1 0 0]))
    (glx-instruction-astorebit nil glx-0 (glx-32 9) glx-0)
    (should (equal *glx-memory* [130 1 0 0]))
    (glx-instruction-astorebit nil glx-1 (glx-32 -1) glx-0)
    (should (equal *glx-memory* [2 1 0 0]))
    (glx-instruction-astorebit nil glx-1 (glx-32 -2) glx-1)
    (should (equal *glx-memory* [66 1 0 0]))
    (glx-instruction-astorebit nil glx-1 (glx-32 -7) glx-0)
    (should (equal *glx-memory* [64 1 0 0]))
    (glx-instruction-astorebit nil glx-1 (glx-32 -8) glx-1)
    (should (equal *glx-memory* [65 1 0 0]))
    (glx-instruction-astorebit nil glx-2 (glx-32 -9) glx-1)
    (should (equal *glx-memory* [193 1 0 0]))
    (glx-instruction-astorebit nil glx-0 (glx-32 21) glx-1)
    (should (equal *glx-memory* [193 1 32 0]))))

(ert-deftest astoreb-instruction ()
  "astoreb instruction"
  :tags '(instructions)
  (let ((*glx-memory* (vector 0 1 2 3 4 5 6 7 8)))
    (glx-instruction-astoreb nil glx-1 glx-2 (glx-32 4 5 6 7))
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
            (glx-instruction-callf nil glx-1 (list #'glx-store-mem glx-4))
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
            (glx-instruction-callfi nil glx-1 glx-4 (list #'glx-store-mem glx-3))
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
            (glx-instruction-callfii nil glx-1 glx-2 glx-3 (list #'glx-store-mem glx-8))
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
            (glx-instruction-callfiii nil glx-0 glx-5 glx-4 glx-3 (list #'glx-store-mem glx-4))
            (should (equal call-args (list glx-0 1 glx-4 (list glx-5 glx-4 glx-3)))))
        
        (advice-remove 'glx-call-function 'test-glx-call-function)))))

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
  (let ((*glx-memory* (make-vector 12 0)))
    (glx-instruction-neg nil glx-5 (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [0 0 0 0 255 255 255 251 0 0 0 0]))
    (glx-instruction-neg nil (glx-32 9 9 9 125) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [0 0 0 0 130 246 246 247 0 0 0 0]))))

(ert-deftest stkcount-instruction ()
  "stkcount instruction"
  :tags '(instructions)

  ;; store to mem
  (let ((*glx-memory* (make-vector 8 0)))
    (cl-letf (((symbol-function 'glx-stack-count) (lambda () glx-2)))
      (glx-instruction-stkcount nil (list #'glx-store-mem glx-4))
      (should (equal *glx-memory* [0 0 0 0 0 0 0 2])))))

(ert-deftest sexb-instruction ()
  "sexb instruction"
  :tags '(instructions)

  (let ((*glx-memory* (make-vector 8 0)))
    (glx-instruction-sexb nil (glx-32 150 1 2 3) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [0 0 0 0 255 255 255 150])))

  (let ((*glx-memory* (make-vector 8 0)))
    (glx-instruction-sexb nil (glx-32 18 1 2 3) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [0 0 0 0 0 0 0 18]))))

(ert-deftest sexs-instruction ()
  "sexs instruction"
  :tags '(instructions)
  
  (let ((*glx-memory* (make-vector 8 0)))
    (glx-instruction-sexs nil (glx-32 150 150 2 3) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [0 0 0 0 255 255 150 150])))

  (let ((*glx-memory* (make-vector 8 0)))
    (glx-instruction-sexs nil (glx-32 18 18 2 3) (list #'glx-store-mem glx-4))
    (should (equal *glx-memory* [0 0 0 0 0 0 18 18]))))
