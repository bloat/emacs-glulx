(context "Glulx glk interface"
         (tag glk)

         (specify "test glx-glk-call"
                  (let (fun-called
                        (*glx-glk-functions* (make-hash-table))
                        (*glx-stack* (list (list (list glx-2 glx-1)))))
                    (puthash #x40 (list #'(lambda (arg1 arg2)
                                            (setq fun-called t)
                                            (expect arg1 equals glx-2)
                                            (expect arg2 equals glx-1)
                                            2)
                                        #'identity #'identity) *glx-glk-functions*)
                    (expect (glx-glk-call #x40 2) equals glx-2)
                    (expect fun-called)))

         (specify "test void return value translates to 0"
                  (let ((*glx-glk-functions* (make-hash-table)))
                    (puthash #x40 (list #'(lambda () nil))
                             *glx-glk-functions*)
                    (expect (glx-glk-call #x40 0) equals glx-0)))

         (specify "test integer return value is converted to 32 bits"
                  (let ((*glx-glk-functions* (make-hash-table)))
                    (puthash #x40 (list #'(lambda () 5))
                             *glx-glk-functions*)
                    (expect (glx-glk-call #x40 0) equals glx-5)))

         (specify "store arg and return value"
                  (let ((*glx-glk-functions* (make-hash-table))
                        (*glx-stack* (list (list (list glx-1))))
                        (*glx-memory* (vector nil nil nil nil nil)))
                    (puthash #x40 (list #'(lambda () (list '\(0\ 0\ 0\ 4\) '\(0\ 0\ 0\ 1\))) (list 1 #'glx-store-mem)) *glx-glk-functions*)
                    (expect (glx-glk-call #x40 1) equals glx-4)
                    (expect *glx-memory* equals [nil 0 0 0 1])))

         (specify "a store value which is a list"
                  (let (fun-called
                        (*glx-glk-functions* (make-hash-table))
                        (*glx-stack* (list (list (list glx-8)))))
                    (puthash #x40 (list #'(lambda () (list 4 '(hello you)))
                                        (list 1 #'(lambda (memptr result)
                                                    (setq fun-called t)
                                                    (expect memptr equals glx-8)
                                                    (expect result equals '(hello you)))))
                             *glx-glk-functions*)
                    (expect (glx-glk-call #x40 1) equals glx-4)
                    (expect fun-called)))

         (specify "arity mismatch"
                  (let ((*glx-glk-functions* (make-hash-table))
                        (*glx-stack* (list (list (list glx-0))))
                        (*glx-memory* [nil nil nil nil]))
                    (puthash #x40 (list #'(lambda () glx-0)) *glx-glk-functions*)
                    (expect (glx-glk-call #x40 1) throws glx-glk-error)))

         (specify "glk calls that need new ids"
                  (let ((*glx-glk-functions* (make-hash-table))
                        (*glx-stack* (list (list (list glx-2 glx-1))))
                        (*glx-glk-id-gen* 0))
                    (puthash #x40 (list #'(lambda (arg1 arg2)
                                            (expect arg1 equals '\(0\ 0\ 0\ 1\))
                                            (expect arg2 equals '\(0\ 0\ 0\ 2\)))
                                        'gen-id 'gen-id) *glx-glk-functions*)
                    (glx-glk-call #x40 0)))

         (specify "glx-32->glk-opq should convert 0 to nil"
                  (expect (not (glx-32->glk-opq glx-0))))

         (specify "glx-32->glk-opq should convert other values to a symbol"
                  (expect (glx-32->glk-opq glx-4) equals '\(0\ 0\ 0\ 4\)))

         (specify "glx-glk-result->32 should convert nil to 0"
                  (expect (glx-glk-result->32 nil) equals glx-0))

         (specify "glx-glk-result->32 should convert glk opaques to 32 bit values"
                  (expect (glx-glk-result->32 '\(0\ 0\ 0\ 5\)) equals glx-5))

         (specify "storing an event into glulx memory"
                  (let ((*glx-memory* (make-vector 33 0)))
                    (glx-glk-store-event `(glk-evtype-lineinput \(0\ 0\ 0\ 1\) 16 0 ,(glx-32 16) "examine building") glx-1)
                    (expect *glx-memory* equals
                            [0 0 0 0 3
                             0 0 0 1
                             0 0 0 16
                             0 0 0 0
                             101 120 97 109 105 110 101 32 98 117 105 108 100 105 110 103])))

         (specify "storing a closed memory stream into glulx memory"
                  (let ((*glx-memory* (make-vector 14 0)))
                    (glx-glk-store-closed-memory-stream glx-0 '(0 5 (0 0 0 9) "hello"))
                    (expect *glx-memory* equals
                            [0 0 0 0
                             0 0 0 5
                             0 104 101 108 108 111])))

         (specify "storing a closed memory stream onto the stack"
                  (let ((*glx-memory* (make-vector 5 0))
                        (*glx-stack* (list (list (list)))))
                    (glx-glk-store-closed-memory-stream (glx-32 -1) '(0 5 (0 0 0 0) "hello"))
                    (expect *glx-memory* equals
                            [104 101 108 108 111])
                    (expect *glx-stack* equals (list (list (list glx-5 glx-0)))))))
