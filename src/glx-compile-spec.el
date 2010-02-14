(context "Compilation of the glulx ROM: "
         (tag compile)

         (specify "Compile the last function"
                  (let ((compiled-function-check nil))
                    (flet ((glx-compile-function (ptr) (setq compiled-function-check ptr) nil))
                      (glx-compile-rom 'a-ptr))
                    (expect compiled-function-check equals 'a-ptr)))

         (specify "Compile the more functions"
                  (let ((compiled-functions-check (list)))
                    (flet ((glx-compile-function (ptr)
                                                 (push ptr compiled-functions-check)
                                                 (cond
                                                  ((eq ptr 'a-ptr) (list 'a-ptr2 'a-ptr3))
                                                  (t nil))))
                      (glx-compile-rom 'a-ptr))
                    (expect compiled-functions-check equals '(a-ptr3 a-ptr2 a-ptr))))

         (specify "Don't recompile functions"
                  (let ((compiled-functions-check (list)))
                    (flet ((glx-compile-function (ptr)
                                                 (push ptr compiled-functions-check)
                                                 (expect (< (length compiled-functions-check) 2))
                                                 (list 'a-ptr)))
                      (glx-compile-rom 'a-ptr))
                    (expect compiled-functions-check equals '(a-ptr))))

         (specify "Compile constant 0 arg"
                  (multiple-value-bind (compiled-arg offset) (glx-compile-load-arg 0 glx-0)
                    (expect (funcall (first compiled-arg) (second compiled-arg)) equals 0)))

         (specify "Compile constant 1 byte arg"
                  (let ((*glx-memory* (vector #x31 #x01 #x4e)))
                    (multiple-value-bind (compiled-arg offset) (glx-compile-load-arg 1 glx-2)
                      (expect (funcall (first compiled-arg) (second compiled-arg)) equals (glx-32 #x4e)))))

         (specify "Compile constant 2 byte arg"
                  (let ((*glx-memory* (vector #x31 #x01 #x4e #x32)))
                    (multiple-value-bind (compiled-arg offset) (glx-compile-load-arg 2 glx-2)
                      (expect (funcall (first compiled-arg) (second compiled-arg)) equals (glx-32 #x32 #x4e)))))

         (specify "Compile constant 4 byte arg"
                  (let ((*glx-memory* (vector #x31 #x01 #x4e #x32 #x00 #xff)))
                    (multiple-value-bind (compiled-arg offset) (glx-compile-load-arg 3 glx-2)
                      (expect (funcall (first compiled-arg) (second compiled-arg)) equals (glx-32 #xff #x00 #x32 #x4e)))))

         (specify "Compile get contents of 1 byte address arg"
                  (let ((*glx-memory* (vector #x31 #x05 #x04 #x00 #x52 #x21 #x45 #xe1)))
                    (multiple-value-bind (compiled-arg offset) (glx-compile-load-arg 5 glx-2)
                      (expect (funcall (first compiled-arg) (second compiled-arg)) equals (glx-32 #xe1 #x45 #x21 #x52)))))

         (specify "Compile get contents of 2 byte address arg"
                  (let ((*glx-memory* (vector #x31 #x06 #x00 #x04 #x52 #x21 #x45 #xe1)))
                    (multiple-value-bind (compiled-arg offset) (glx-compile-load-arg 6 glx-2)
                      (expect (funcall (first compiled-arg) (second compiled-arg)) equals (glx-32 #xe1 #x45 #x21 #x52)))))

         (specify "Compile get contents of 4 byte address arg"
                  (let ((*glx-memory* (vector #x31 #x07 #x00 #x00 #x00 #x02)))
                    (multiple-value-bind (compiled-arg offset) (glx-compile-load-arg 7 glx-2)
                      (expect (funcall (first compiled-arg) (second compiled-arg)) equals glx-2))))

         (specify "Compile stack arg"
                  (multiple-value-bind (compiled-arg offset) (glx-compile-load-arg 8 nil)
                    (let ((*glx-stack* `(((,glx-5) ()))))
                      (expect (funcall (first compiled-arg) (second compiled-arg)) equals glx-5))))

         (specify "Compile locals arg (one byte offset)"
                  (let ((*glx-memory* (vector #x31 #x09 #x03)))
                    (multiple-value-bind (compiled-arg offset) (glx-compile-load-arg 9 glx-2)
                      (let ((*glx-stack* (list (list nil (list (cons glx-3 glx-4))))))
                        (expect (funcall (first compiled-arg) (second compiled-arg)) equals glx-4)))))

         (specify "Compile locals arg (two byte offset)"
                  (let ((*glx-memory* (vector #x31 #x0a #x32 #x03)))
                    (multiple-value-bind (compiled-arg offset) (glx-compile-load-arg 10 glx-2)
                      (let ((*glx-stack* (list (list nil (list (cons (glx-32 #x03 #x32) glx-4))))))
                        (expect (funcall (first compiled-arg) (second compiled-arg)) equals glx-4)))))

         (specify "Compile locals arg (four byte offset)"
                  (let ((*glx-memory* (vector #x31 #x0a #x34 #x22 #x32 #x03)))
                    (multiple-value-bind (compiled-arg offset) (glx-compile-load-arg 11 glx-2)
                      (let ((*glx-stack* (list (list nil (list (cons (glx-32 #x03 #x32 #x22 #x34) glx-4))))))
                        (expect (funcall (first compiled-arg) (second compiled-arg)) equals glx-4)))))

         (specify "Compile a RAM arg (one byte offset)"
                  (let ((*glx-memory* (vector #x31 #x0d #x02 #xee #x32 #x03 #x45))
                        (*glx-ram-start* glx-1))
                    (multiple-value-bind (compiled-arg offset) (glx-compile-load-arg 13 glx-2)
                      (expect (funcall (first compiled-arg) (second compiled-arg)) equals (glx-32 #x45 3 #x32 #xee)))))

         (specify "Compile a RAM arg (two byte offset)"
                  (let ((*glx-memory* (vector #x31 #x0e 0 #x03 #xee #x32 #x03 #x45))
                        (*glx-ram-start* glx-1))
                    (multiple-value-bind (compiled-arg offset) (glx-compile-load-arg 14 glx-2)
                      (expect (funcall (first compiled-arg) (second compiled-arg)) equals (glx-32 #x45 3 #x32 #xee)))))

         (specify "Compile a RAM arg (four byte offset)"
                  (let ((*glx-memory* (vector #x31 #x0f 0 0 0 #x05 #xee #x32 #x03 #x45))
                        (*glx-ram-start* glx-1))
                    (multiple-value-bind (compiled-arg offset) (glx-compile-load-arg 15 glx-2)
                      (expect (funcall (first compiled-arg) (second compiled-arg)) equals (glx-32 #x45 3 #x32 #xee)))))

         (specify "Compile instruction (no args)"
                  (let ((*glx-memory* (vector #x00)))
                    (multiple-value-bind (next-inst compiled-fun)
                        (glx-compile-instruction glx-0)
                      (expect next-inst equals glx-1)
                      (glx-execute-compiled-instruction compiled-fun))))

         (specify "Compile instruction (no args)"
                  (let ((*glx-memory* (vector #x00)))
                    (multiple-value-bind (next-inst compiled-fun)
                        (glx-compile-instruction glx-0)
                      (expect next-inst equals glx-1)
                      (glx-execute-compiled-instruction compiled-fun))))

         (specify "Compile instruction (no args) with effect"
                  (let ((*glx-memory* (vector #x120)))
                    (multiple-value-bind (next-inst compiled-fun)
                        (glx-compile-instruction glx-0)
                      (expect next-inst equals glx-1)
                      (expect (glx-execute-compiled-instruction compiled-fun) equals 'glx-quit))))

         (specify "Compile instruction (one load arg [constant])"
                  (let ((*glx-memory* (vector #x20 #x01 #x66)))
                    (multiple-value-bind (next-inst compiled-fun)
                        (glx-compile-instruction glx-0)
                      (expect next-inst equals glx-3)
                      (let ((*glx-pc* (glx-32 100))
                            (*glx-memory* (vector #x20 #x01 #x33))) ; constants are compiled before run time
                        (glx-execute-compiled-instruction compiled-fun)
                        (expect *glx-pc* equals (glx-32 200))))))

         (specify "Compile instruction (one load arg [from memory])"
                  (let ((*glx-memory* (vector #x20 #x05 #x03 0 0 0 #x66)))
                    (multiple-value-bind (next-inst compiled-fun)
                        (glx-compile-instruction glx-0)
                      (expect next-inst equals glx-3)
                      (let ((*glx-pc* (glx-32 100))
                            (*glx-memory* (vector #x20 #x05 #x03 0 0 0 #x33)))
                        (glx-execute-compiled-instruction compiled-fun)
                        (expect *glx-pc* equals (glx-32 149))))))

         (specify "Compile instruction (one store arg)"
                  (let ((*glx-memory* (vector #x10 #x98 #x0d #x04 #x02))
                        (*glx-ram-start* glx-5))
                    (multiple-value-bind (next-inst compiled-fun)
                        (glx-compile-instruction glx-0)
                      (expect next-inst equals glx-5)
                      (let ((*glx-pc* (glx-32 100))
                            (*glx-memory* (vector #x10 #x98 #x0d #x04 #x02 0 0 0 0 0 0))
                            (*glx-stack* (list (list (list glx-8) (list (cons glx-4 glx-3))))))
                        (glx-execute-compiled-instruction compiled-fun)
                        (expect *glx-memory* equals [#x10 #x98 #x0d #x04 #x02 0 0 0 0 0 11]))))))
