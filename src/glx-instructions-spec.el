(context "Instruction implementations"
         (tag inst)


         (specify "call instruction"
                  (let ((call-args nil))
                    (flet ((glx-call-function (fptr dt da args) (setq call-args (list fptr dt da args))))

                      ;; zero args call - ignore result
                      (glx-instruction-call glx-0 glx-0 (list #'glx-store-throw nil))
                      (expect call-args equals (list glx-0 0 glx-0 nil))

                      ;; two args call - store result in memory
                      (let ((*glx-stack* (list (list (list glx-2 glx-3)))))
                        (glx-instruction-call glx-0 glx-2 (list #'glx-store-mem glx-4))
                        (expect call-args equals (list glx-0 1 glx-4 (list glx-2 glx-3)))
                        (expect *glx-stack* equals '((()))))

                      ;; zero args call - push result
                      (glx-instruction-call glx-0 glx-0 (list #'glx-store-stack nil))
                      (expect call-args equals (list glx-0 3 glx-0 nil))

                      ;; zero args call - store result in a local
                      (glx-instruction-call glx-0 glx-0 (list #'glx-store-local glx-5))
                      (expect call-args equals (list glx-0 2 glx-5 nil)))))

         (specify "copy instruction"

                  ;; copy to stack
                  (let ((*glx-stack* (list (list nil))))
                    (glx-instruction-copy glx-2 (list #'glx-store-stack nil))
                    (expect *glx-stack* equals (list (list (list glx-2)))))

                  ;; copy to local
                  (let ((*glx-stack* (list (list nil (list (cons glx-2 glx-0))))))
                    (glx-instruction-copy glx-2 (list #'glx-store-local glx-2))
                    (expect *glx-stack* equals (list (list nil (list (cons glx-2 glx-2)))))))

         (specify "sub instruction"

                  ;; store to mem
                  (let ((*glx-memory* (make-vector 12 0)))
                    (glx-instruction-sub glx-2 glx-1 (list #'glx-store-mem glx-4))
                    (expect *glx-memory* equals [0 0 0 0 0 0 0 1 0 0 0 0])))

         (specify "jlt instruction"

                  (let ((*glx-pc* glx-0))
                    (glx-instruction-jlt glx-2 glx-1 glx-5)
                    (expect *glx-pc* equals glx-0)
                    (glx-instruction-jlt glx-1 glx-2 glx-5)
                    (expect *glx-pc* equals glx-3)
                    (glx-instruction-jlt glx-1 glx-1 glx-8)
                    (expect *glx-pc* equals glx-3)
                    (glx-instruction-jlt (glx-32 -1) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals glx-3)
                    (glx-instruction-jlt (glx-32 -2) (glx-32 -1) glx-8)
                    (expect *glx-pc* equals (glx-32 9))
                    (glx-instruction-jlt (glx-32 -2) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals (glx-32 9))
                    (glx-instruction-jlt (glx-32 -2) (glx-32 1) glx-8)
                    (expect *glx-pc* equals (glx-32 15)))

                  (let (return-was-called)
                    (flet ((glx-instruction-return (result) (setq return-was-called result)))
                      (glx-instruction-jlt glx-1 glx-2 glx-1)
                      (expect return-was-called equals glx-1))))

         (specify "getmemsize instruction"

                  (let ((*glx-memory* (make-vector 8 0)))
                    (glx-instruction-getmemsize (list #'glx-store-mem glx-1))
                    (expect *glx-memory* equals [0 0 0 0 8 0 0 0])))

         (specify "jne instruction"

                  (let ((*glx-pc* glx-4))
                    (glx-instruction-jne glx-2 glx-1 glx-5)
                    (expect *glx-pc* equals (glx-32 7))
                    (glx-instruction-jne glx-1 glx-1 glx-5)
                    (expect *glx-pc* equals (glx-32 7)))

                  (let (return-was-called)
                    (flet ((glx-instruction-return (result) (setq return-was-called result)))
                      (glx-instruction-jne glx-1 glx-2 glx-0)
                      (expect return-was-called equals glx-0))))

         (specify "return instruction"

                  (let ((call-count 0))
                    (flet ((glx-return-from-function
                            () (cond ((= call-count 0) (incf call-count) (list 0 glx-0 glx-5))
                                     ((= call-count 1) (incf call-count) (list 3 glx-0 glx-5))
                                     ((= call-count 2) (incf call-count) (list 2 glx-2 glx-5))
                                     ((= call-count 3) (incf call-count) (list 1 glx-2 glx-5)))))

                      ;; ignore result
                      (glx-instruction-return glx-3)
                      (expect call-count equals 1)

                      ;; push result onto stack
                      (let ((*glx-stack* (list (list nil))))
                        (glx-instruction-return glx-4)
                        (expect *glx-stack* equals (list (list (list glx-4))))
                        (expect call-count equals 2))

                      ;; store result in local
                      (let ((*glx-stack* (list (list nil (list (cons glx-2 glx-0))))))
                        (glx-instruction-return glx-4)
                        (expect *glx-stack* equals (list (list nil (list (cons glx-2 glx-4)))))
                        (expect call-count equals 3))

                      ;; store result in memory
                      (let ((*glx-memory* (make-vector 8 0)))
                        (glx-instruction-return (glx-32 4 5 6 7))
                        (expect *glx-memory* equals [0 0 7 6 5 4 0 0])
                        (expect call-count equals 4)))))

         (specify "jge instruction"

                  (let ((*glx-pc* glx-0))
                    (glx-instruction-jge glx-2 glx-1 glx-5)
                    (expect *glx-pc* equals glx-3)
                    (glx-instruction-jge glx-1 glx-2 glx-5)
                    (expect *glx-pc* equals glx-3)
                    (glx-instruction-jge glx-1 glx-1 glx-8)
                    (expect *glx-pc* equals (glx-32 9))
                    (glx-instruction-jge (glx-32 -1) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals (glx-32 15))
                    (glx-instruction-jge (glx-32 -2) (glx-32 -1) glx-8)
                    (expect *glx-pc* equals (glx-32 15))
                    (glx-instruction-jge (glx-32 -2) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals (glx-32 21))
                    (glx-instruction-jge (glx-32 1) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals (glx-32 27))))

         (specify "jle instruction"

                  (let ((*glx-pc* glx-0))
                    (glx-instruction-jle glx-2 glx-1 glx-5)
                    (expect *glx-pc* equals glx-0)
                    (glx-instruction-jle glx-1 glx-2 glx-5)
                    (expect *glx-pc* equals glx-3)
                    (glx-instruction-jle glx-1 glx-1 glx-8)
                    (expect *glx-pc* equals (glx-32 9))
                    (glx-instruction-jle (glx-32 -1) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals (glx-32 9))
                    (glx-instruction-jle (glx-32 -2) (glx-32 -1) glx-8)
                    (expect *glx-pc* equals (glx-32 15))
                    (glx-instruction-jle (glx-32 -2) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals (glx-32 21))
                    (glx-instruction-jle (glx-32 1) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals (glx-32 21))))

         (specify "aloadb instruction"

                  (let ((*glx-memory* (vector 0 1 2 3 4 5 6 7 8)))
                    (glx-instruction-aloadb glx-2 glx-1 (list #'glx-store-mem glx-4))
                    (expect *glx-memory* [0 1 2 3 0 0 0 3 8])))

         (specify "jgt instruction"

                  (let ((*glx-pc* glx-0))
                    (glx-instruction-jgt glx-2 glx-1 glx-5)
                    (expect *glx-pc* equals glx-3)
                    (glx-instruction-jgt glx-1 glx-2 glx-5)
                    (expect *glx-pc* equals glx-3)
                    (glx-instruction-jgt glx-1 glx-1 glx-8)
                    (expect *glx-pc* equals glx-3)
                    (glx-instruction-jgt (glx-32 -1) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals (glx-32 9))
                    (glx-instruction-jgt (glx-32 -2) (glx-32 -1) glx-8)
                    (expect *glx-pc* equals (glx-32 9))
                    (glx-instruction-jgt (glx-32 -2) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals (glx-32 9))
                    (glx-instruction-jgt (glx-32 1) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals (glx-32 15))))

         (specify "aload instruction"

                  (let ((*glx-memory* [0 1 2 3 4 5 6 7 8 9 10])
                        (*glx-stack* (list (list nil))))
                    (glx-instruction-aload glx-2 glx-1 (list #'glx-store-stack nil))
                    (expect *glx-stack* equals (list (list (list (glx-32 9 8 7 6)))))))

         (specify "jeq instruction"

                  (let ((*glx-pc* glx-0))
                    (glx-instruction-jeq glx-2 glx-1 glx-5)
                    (expect *glx-pc* equals glx-0)
                    (glx-instruction-jeq glx-1 glx-2 glx-5)
                    (expect *glx-pc* equals glx-0)
                    (glx-instruction-jeq glx-1 glx-1 glx-8)
                    (expect *glx-pc* equals (glx-32 6))
                    (glx-instruction-jeq (glx-32 -1) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals (glx-32 6))
                    (glx-instruction-jeq (glx-32 -2) (glx-32 -1) glx-8)
                    (expect *glx-pc* equals (glx-32 6))
                    (glx-instruction-jeq (glx-32 -2) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals (glx-32 12))))

         (specify "jz instruction"

                  (let ((*glx-pc* glx-0))
                    (glx-instruction-jz glx-0 glx-5)
                    (expect *glx-pc* equals glx-3)
                    (glx-instruction-jz glx-1 glx-5)
                    (expect *glx-pc* equals glx-3)
                    (glx-instruction-jz (glx-32 -1) glx-8)
                    (expect *glx-pc* equals glx-3)))

         (specify "jnz instruction"

                  (let ((*glx-pc* glx-0))
                    (glx-instruction-jnz glx-0 glx-5)
                    (expect *glx-pc* equals glx-0)
                    (glx-instruction-jnz glx-1 glx-5)
                    (expect *glx-pc* equals glx-3)
                    (glx-instruction-jnz (glx-32 -1) glx-8)
                    (expect *glx-pc* equals (glx-32 9))))

         (specify "add instruction"

                  ;; store to mem
                  (let ((*glx-memory* (make-vector 12 0)))
                    (glx-instruction-add glx-5 glx-8 (list #'glx-store-mem glx-4))
                    (expect *glx-memory* equals [0 0 0 0 0 0 0 13 0 0 0 0])))

         (specify "aloadbit instruction"

                  ;; store to mem
                  (let ((*glx-memory* (vector 130 1 0 0 255 255 255 255)))
                    (glx-instruction-aloadbit glx-0 glx-0 (list #'glx-store-mem glx-4))
                    (expect *glx-memory* equals [130 1 0 0 0 0 0 0])
                    (glx-instruction-aloadbit glx-0 glx-1 (list #'glx-store-mem glx-4))
                    (expect *glx-memory* equals [130 1 0 0 0 0 0 1])
                    (setq *glx-memory* [130 1 0 0 255 255 255 255])
                    (glx-instruction-aloadbit glx-0 (glx-32 7) (list #'glx-store-mem glx-4))
                    (expect *glx-memory* equals [130 1 0 0 0 0 0 1])
                    (setq *glx-memory* [130 1 0 0 255 255 255 255])
                    (glx-instruction-aloadbit glx-0 glx-8 (list #'glx-store-mem glx-4))
                    (expect *glx-memory* equals [130 1 0 0 0 0 0 1])
                    (glx-instruction-aloadbit glx-0 (glx-32 9) (list #'glx-store-mem glx-4))
                    (expect *glx-memory* equals [130 1 0 0 0 0 0 0])
                    (glx-instruction-aloadbit glx-1 (glx-32 -1) (list #'glx-store-mem glx-4))
                    (expect *glx-memory* equals [130 1 0 0 0 0 0 1])
                    (glx-instruction-aloadbit glx-1 (glx-32 -2) (list #'glx-store-mem glx-4))
                    (expect *glx-memory* equals [130 1 0 0 0 0 0 0])
                    (glx-instruction-aloadbit glx-1 (glx-32 -7) (list #'glx-store-mem glx-4))
                    (expect *glx-memory* equals [130 1 0 0 0 0 0 1])
                    (setq *glx-memory* [130 1 0 0 255 255 255 255])
                    (glx-instruction-aloadbit glx-1 (glx-32 -8) (list #'glx-store-mem glx-4))
                    (expect *glx-memory* equals [130 1 0 0 0 0 0 0])
                    (glx-instruction-aloadbit glx-2 (glx-32 -9) (list #'glx-store-mem glx-4))
                    (expect *glx-memory* equals [130 1 0 0 0 0 0 1]))
                  (let ((*glx-memory* (vector 0 0 32 0 0 0 0 0)))
                    (glx-instruction-aloadbit glx-0 (glx-32 21) (list #'glx-store-mem glx-4))
                    (expect *glx-memory* equals [0 0 32 0 0 0 0 1])
                    (setq *glx-memory* [0 0 32 0 255 255 255 255])
                    (glx-instruction-aloadbit (glx-32 6) (glx-32 -27) (list #'glx-store-mem glx-4))
                    (expect *glx-memory* equals [0 0 32 0 0 0 0 1])))

         (specify "aloads instruction"
                  (let ((*glx-memory* [1 2 3 4 5 6 7 8 9])
                        (*glx-stack* (list (list nil))))
                    (glx-instruction-aloads glx-0 glx-0 (list #'glx-store-stack nil))
                    (expect *glx-stack* equals `(((,(glx-32 2 1)))))
                    (setq *glx-stack* (list (list nil)))
                    (glx-instruction-aloads glx-0 glx-3 (list #'glx-store-stack nil))
                    (expect *glx-stack* equals `(((,(glx-32 8 7)))))
                    (setq *glx-stack* (list (list nil)))
                    (glx-instruction-aloads glx-4 glx-1 (list #'glx-store-stack nil))
                    (expect *glx-stack* equals `(((,(glx-32 8 7)))))))

         (specify "mul instruction"

                  ;; store to mem
                  (let ((*glx-memory* (make-vector 12 0)))
                    (glx-instruction-mul glx-5 glx-8 (list #'glx-store-mem glx-4))
                    (expect *glx-memory* equals [0 0 0 0 0 0 0 40 0 0 0 0])))

         (specify "stkcopy instruction"

                  (let ((*glx-stack* (list (list (list glx-0 glx-1 glx-2 glx-3)))))
                    (glx-instruction-stkcopy glx-3)
                    (expect *glx-stack* equals (list (list (list glx-0 glx-1 glx-2 glx-0 glx-1 glx-2 glx-3))))))

         (specify "gestalt instruction"

                  (let ((*glx-memory* (make-vector 5 0)))
                    (glx-instruction-gestalt glx-0 glx-0 (list #'glx-store-mem glx-1))
                    (expect *glx-memory* equals [0 0 3 1 0])
                    (glx-instruction-gestalt glx-1 glx-0 (list #'glx-store-mem glx-1))
                    (expect *glx-memory* equals [0 0 0 0 0])
                    (glx-instruction-gestalt glx-2 glx-0 (list #'glx-store-mem glx-1))
                    (expect *glx-memory* equals [0 0 0 0 1])
                    (glx-instruction-gestalt glx-3 glx-0 (list #'glx-store-mem glx-1))
                    (expect *glx-memory* equals [0 0 0 0 0])
                    (glx-instruction-gestalt glx-4 glx-0 (list #'glx-store-mem glx-1))
                    (expect *glx-memory* equals [0 0 0 0 1])
                    (glx-instruction-gestalt glx-4 glx-2 (list #'glx-store-mem glx-1))
                    (expect *glx-memory* equals [0 0 0 0 1])
                    (glx-instruction-gestalt glx-4 glx-2 (list #'glx-store-mem glx-1))
                    (expect *glx-memory* equals [0 0 0 0 1])
                    (glx-instruction-gestalt glx-4 glx-3 (list #'glx-store-mem glx-1))
                    (expect *glx-memory* equals [0 0 0 0 0])))

         (specify "setiosys"
                  (let ((*glx-glk-selected* nil))
                    (glx-instruction-setiosys glx-2 glx-0)
                    (expect *glx-glk-selected*)
                    (expect (glx-instruction-setiosys glx-3 glx-0) throws 'glx-glk-error)))

         (specify "jump instruction"
                  (let ((*glx-pc* glx-0))
                    (glx-instruction-jump glx-5)
                    (expect *glx-pc* equals glx-3)))

         (specify "astore instruction"
                  (let ((*glx-memory* (make-vector 16 0)))
                    (glx-instruction-astore glx-4 glx-2 glx-8)
                    (expect *glx-memory* equals [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8])))

         (specify "copys instruction"

                  ;; copy to stack
                  (let ((*glx-stack* (list (list nil))))
                    (glx-instruction-copys (glx-32 7 6 5 4) (list #'glx-store-stack nil))
                    (expect *glx-stack* equals (list (list (list (glx-32 5 4))))))

                  ;; copy to local
                  (let ((*glx-stack* (list (list nil (list (cons glx-2 glx-0))))))
                    (glx-instruction-copys (glx-32 9 8 7 6) (list #'glx-store-local glx-2))
                    (expect *glx-stack* equals (list (list nil (list (cons glx-2 (glx-32 7 6)))))))

                  ;; copy to memory
                  (let ((*glx-memory* [0 0 0 0]))
                    (glx-instruction-copys (glx-32 9 8 7 6) (list #'glx-store-mem glx-2))
                    (expect *glx-memory* equals [0 0 6 7])))

         (specify "copyb instruction"

                  ;; copy to stack
                  (let ((*glx-stack* (list (list nil))))
                    (glx-instruction-copyb (glx-32 7 6 5 4) (list #'glx-store-stack nil))
                    (expect *glx-stack* equals (list (list (list (glx-32 4))))))

                  ;; copy to local
                  (let ((*glx-stack* (list (list nil (list (cons glx-2 glx-0))))))
                    (glx-instruction-copyb (glx-32 9 8 7 6) (list #'glx-store-local glx-2))
                    (expect *glx-stack* equals (list (list nil (list (cons glx-2 (glx-32 6)))))))

                  ;; copy to memory
                  (let ((*glx-memory* [0 0 0 0]))
                    (glx-instruction-copyb (glx-32 9 8 7 6) (list #'glx-store-mem glx-2))
                    (expect *glx-memory* equals [0 0 6 0])))

         (specify "astorebit instruction"

                  ;; store to mem
                  (let ((*glx-memory* (vector 130 1 0 0)))
                    (glx-instruction-astorebit glx-0 glx-0 glx-1)
                    (expect *glx-memory* equals [131 1 0 0])
                    (glx-instruction-astorebit glx-0 glx-0 glx-0)
                    (expect *glx-memory* equals [130 1 0 0])
                    (glx-instruction-astorebit glx-0 (glx-32 7) glx-0)
                    (expect *glx-memory* equals [2 1 0 0])
                    (glx-instruction-astorebit glx-0 (glx-32 7) glx-1)
                    (expect *glx-memory* equals [130 1 0 0])
                    (glx-instruction-astorebit glx-0 (glx-32 9) glx-0)
                    (expect *glx-memory* equals [130 1 0 0])
                    (glx-instruction-astorebit glx-1 (glx-32 -1) glx-0)
                    (expect *glx-memory* equals [2 1 0 0])
                    (glx-instruction-astorebit glx-1 (glx-32 -2) glx-1)
                    (expect *glx-memory* equals [66 1 0 0])
                    (glx-instruction-astorebit glx-1 (glx-32 -7) glx-0)
                    (expect *glx-memory* equals [64 1 0 0])
                    (glx-instruction-astorebit glx-1 (glx-32 -8) glx-1)
                    (expect *glx-memory* equals [65 1 0 0])
                    (glx-instruction-astorebit glx-2 (glx-32 -9) glx-1)
                    (expect *glx-memory* equals [193 1 0 0])
                    (glx-instruction-astorebit glx-0 (glx-32 21) glx-1)
                    (expect *glx-memory* equals [193 1 32 0])))

         (specify "astoreb instruction"
                  (let ((*glx-memory* (vector 0 1 2 3 4 5 6 7 8)))
                    (glx-instruction-astoreb glx-1 glx-2 (glx-32 4 5 6 7))
                    (expect *glx-memory* equals [0 1 2 4 4 5 6 7 8])))

         (specify "callf instruction"
                  (let ((call-args nil))
                    (flet ((glx-call-function (fptr dt da args) (setq call-args (list fptr dt da args))))

                      ;; store result in memory
                      (glx-instruction-callf glx-1 (list #'glx-store-mem glx-4))
                      (expect call-args equals (list glx-1 1 glx-4 nil)))))

         (specify "callfi instruction"
                  (let ((call-args nil))
                    (flet ((glx-call-function (fptr dt da args) (setq call-args (list fptr dt da args))))

                      ;; store result in memory
                      (glx-instruction-callfi glx-1 glx-4 (list #'glx-store-mem glx-3))
                      (expect call-args equals (list glx-1 1 glx-3 (list glx-4))))))

         (specify "callfii instruction"
                  (let ((call-args nil))
                    (flet ((glx-call-function (fptr dt da args) (setq call-args (list fptr dt da args))))

                      ;; store result in memory
                      (glx-instruction-callfii glx-1 glx-2 glx-3 (list #'glx-store-mem glx-8))
                      (expect call-args equals (list glx-1 1 glx-8 (list glx-2 glx-3))))))

         (specify "callfiii instruction"
                  (let ((call-args nil))
                    (flet ((glx-call-function (fptr dt da args) (setq call-args (list fptr dt da args))))

                      ;; store result in memory
                      (glx-instruction-callfiii glx-0 glx-5 glx-4 glx-3 (list #'glx-store-mem glx-4))
                      (expect call-args equals (list glx-0 1 glx-4 (list glx-5 glx-4 glx-3))))))

         (specify "jgeu instruction"

                  (let ((*glx-pc* glx-0))
                    (glx-instruction-jgeu glx-2 glx-1 glx-5)
                    (expect *glx-pc* equals glx-3)
                    (glx-instruction-jgeu glx-1 glx-2 glx-5)
                    (expect *glx-pc* equals glx-3)
                    (glx-instruction-jgeu glx-1 glx-1 glx-8)
                    (expect *glx-pc* equals (glx-32 9))
                    (glx-instruction-jgeu (glx-32 -1) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals (glx-32 15))
                    (glx-instruction-jgeu (glx-32 -2) (glx-32 -1) glx-8)
                    (expect *glx-pc* equals (glx-32 15))
                    (glx-instruction-jgeu (glx-32 -2) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals (glx-32 21))
                    (glx-instruction-jgeu (glx-32 1) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals (glx-32 21))))

         (specify "jgtu instruction"

                  (let ((*glx-pc* glx-0))
                    (glx-instruction-jgtu glx-2 glx-1 glx-5)
                    (expect *glx-pc* equals glx-3)
                    (glx-instruction-jgtu glx-1 glx-2 glx-5)
                    (expect *glx-pc* equals glx-3)
                    (glx-instruction-jgtu glx-1 glx-1 glx-8)
                    (expect *glx-pc* equals glx-3)
                    (glx-instruction-jgtu (glx-32 -1) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals (glx-32 9))
                    (glx-instruction-jgtu (glx-32 -2) (glx-32 -1) glx-8)
                    (expect *glx-pc* equals (glx-32 9))
                    (glx-instruction-jgtu (glx-32 -2) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals (glx-32 9))
                    (glx-instruction-jgtu (glx-32 1) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals (glx-32 9))))

         (specify "jltu instruction"

                  (let ((*glx-pc* glx-0))
                    (glx-instruction-jltu glx-2 glx-1 glx-5)
                    (expect *glx-pc* equals glx-0)
                    (glx-instruction-jltu glx-1 glx-2 glx-5)
                    (expect *glx-pc* equals glx-3)
                    (glx-instruction-jltu glx-1 glx-1 glx-8)
                    (expect *glx-pc* equals glx-3)
                    (glx-instruction-jltu (glx-32 -1) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals glx-3)
                    (glx-instruction-jltu (glx-32 -2) (glx-32 -1) glx-8)
                    (expect *glx-pc* equals (glx-32 9))
                    (glx-instruction-jltu (glx-32 -2) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals (glx-32 9))
                    (glx-instruction-jltu (glx-32 -2) (glx-32 1) glx-8)
                    (expect *glx-pc* equals (glx-32 9))))

         (specify "jleu instruction"

                  (let ((*glx-pc* glx-0))
                    (glx-instruction-jleu glx-2 glx-1 glx-5)
                    (expect *glx-pc* equals glx-0)
                    (glx-instruction-jleu glx-1 glx-2 glx-5)
                    (expect *glx-pc* equals glx-3)
                    (glx-instruction-jleu glx-1 glx-1 glx-8)
                    (expect *glx-pc* equals (glx-32 9))
                    (glx-instruction-jleu (glx-32 -1) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals (glx-32 9))
                    (glx-instruction-jleu (glx-32 -2) (glx-32 -1) glx-8)
                    (expect *glx-pc* equals (glx-32 15))
                    (glx-instruction-jleu (glx-32 -2) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals (glx-32 21))
                    (glx-instruction-jleu (glx-32 1) (glx-32 -2) glx-8)
                    (expect *glx-pc* equals (glx-32 27))))

         (specify "neg instruction"

                  ;; store to mem
                  (let ((*glx-memory* (make-vector 12 0)))
                    (glx-instruction-neg glx-5 (list #'glx-store-mem glx-4))
                    (expect *glx-memory* equals [0 0 0 0 255 255 255 251 0 0 0 0])
                    (glx-instruction-neg (glx-32 9 9 9 125) (list #'glx-store-mem glx-4))
                    (expect *glx-memory* equals [0 0 0 0 130 246 246 247 0 0 0 0]))))
