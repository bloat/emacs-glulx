(context "Execute the opcodes"
         (tag exec)

         (specify "Get opcode number"
                  (let ((*glx-memory* (vector #x45 #x9C #x34 #xC0 #x34 #x03 #xE9)))
                    (expect (glx-get-opcode glx-0) equals (list glx-1 #x45))
                    (expect (glx-get-opcode glx-1) equals (list glx-3 #x1c34))
                    (expect (glx-get-opcode glx-3) equals (list (glx-32 7) #x3403e9))))

         (specify "Get args for opcode with no args"
                  (let ((*glx-memory* (vector #x00 #x45)))
                    (expect (glx-get-opcode-args #x00 glx-1) equals (list glx-1 ()))))

         (specify "Get constant zero arg (one arg opcode)"
                  (let ((*glx-memory* (vector #x31 #x00)))
                    (expect (glx-get-opcode-args #x31 glx-1) equals (list glx-2 (list glx-0)))))

         (specify "Get constant one byte arg (one arg opcode)"
                  (let ((*glx-memory* (vector #x31 #x01 #xde)))
                    (expect (glx-get-opcode-args #x31 glx-1) equals (list glx-3 (list (glx-32 -34))))))

         (specify "Get constant two byte arg (one arg opcode)"
                  (let ((*glx-memory* (vector #x31 #x02 #xde #x4c)))
                    (expect (glx-get-opcode-args #x31 glx-1) equals (list glx-4 (list (glx-32 -8628))))))

         (specify "Get constant 4 byte arg (one arg opcode)"
                  (let ((*glx-memory* (vector #x31 #x03 #x01 #x45 #x23 #xce)))
                    (expect (glx-get-opcode-args #x31 glx-1) equals (list (glx-32 6) (list (glx-32 #x014523ce))))))

         (specify "Get contents of 1 byte address arg (one arg opcode)"
                  (let ((*glx-memory* (vector #x31 #x05 #x04 #x00 #x52 #x21 #x45 #xe1)))
                    (expect (glx-get-opcode-args #x31 glx-1) equals (list glx-3 (list (glx-32 #xe1 #x45 #x21 #x52))))))

         (specify "Get contents of 2 byte address arg (one arg opcode)"
                  (let ((*glx-memory* (vector #x31 #x06 #x00 #x04 #x52 #x21 #x45 #xe1)))
                    (expect (glx-get-opcode-args #x31 glx-1) equals (list glx-4 (list (glx-32 #xe1 #x45 #x21 #x52))))))

         (specify "Get contents of 4 byte address arg (one arg opcode)"
                  (let ((*glx-memory* (vector #x31 #x07 #x00 #x00 #x00 #x02)))
                    (expect (glx-get-opcode-args #x31 glx-1) equals (list (glx-32 6) (list glx-2)))))

         (specify "Get an arg from the stack (one arg opcode)"
                  (let ((*glx-memory* (vector #x31 #x08))
                        (*glx-stack* `(((,glx-5) ()))))
                    (expect (glx-get-opcode-args #x31 glx-1) equals (list glx-2 (list glx-5)))))

         (specify "Get an arg from the locals list (one byte offset) (one arg opcode)"
                  (let ((*glx-memory* (vector #x31 #x09 #x03))
                        (*glx-stack* (list (list nil (list (cons glx-3 glx-4))))))
                    (expect (glx-get-opcode-args #x31 glx-1) equals (list glx-3 (list glx-4)))))

         (specify "Get an arg from the locals list (two byte offset) (one arg opcode)"
                  (let ((*glx-memory* (vector #x31 #x0a #x32 #x03))
                        (*glx-stack* (list (list nil (list (cons (glx-32 3 #x32) glx-5))))))
                    (expect (glx-get-opcode-args #x31 glx-1) equals (list glx-4 (list glx-5)))))

         (specify "Get an arg from the locals list (four byte offset) (one arg opcode)"
                  (let ((*glx-memory* (vector #x31 #x0b #xc1 #xee #x32 #x03))
                        (*glx-stack* (list (list nil (list (cons (glx-32 3 #x32 #xee #xc1) glx-2))))))
                    (expect (glx-get-opcode-args #x31 glx-1) equals (list (glx-32 6) (list glx-2)))))

         (specify "Get an arg from RAM (one byte offset) (one arg opcode)"
                  (let ((*glx-memory* (vector #x31 #x0d #x02 #xee #x32 #x03 #x45))
                        (*glx-ram-start* glx-1))
                    (expect (glx-get-opcode-args #x31 glx-1) equals (list glx-3 (list (glx-32 #x45 3 #x32 #xee))))))

         (specify "Get an arg from RAM (two byte offset) (one arg opcode)"
                  (let ((*glx-memory* (vector #x31 #x0e 0 #x03 #xee #x32 #x03 #x45))
                        (*glx-ram-start* glx-1))
                    (expect (glx-get-opcode-args #x31 glx-1) equals (list glx-4 (list (glx-32 #x45 3 #x32 #xee))))))

         (specify "Get an arg from RAM (four byte offset) (one arg opcode)"
                  (let ((*glx-memory* (vector #x31 #x0f 0 0 0 #x05 #xee #x32 #x03 #x45))
                        (*glx-ram-start* glx-1))
                    (expect (glx-get-opcode-args #x31 glx-1) equals (list (glx-32 6) (list (glx-32 #x45 3 #x32 #xee))))))

         (specify "Get a few different kinds of args (three arg opcode)"
                  (let ((*glx-memory* (vector #x23 #xe0 #x09 0 #x4 #x16 #x34 #x23 #x44 #x56))
                        (*glx-stack* (list (list nil (list (cons (glx-32 #x16) glx-4)))))
                        (*glx-ram-start* glx-2))
                    (expect (glx-get-opcode-args #x25 glx-1) equals (list (glx-32 6) (list glx-0 (glx-32 #x56 #x44 #x23 #x34) glx-4)))))

         (specify "Get a store arg - store to nowhere"
                  (let ((*glx-memory* (vector #x102 #x00)))
                    (expect (glx-get-opcode-args #x102 glx-1) equals (list glx-2 (list (list #'glx-store-throw nil))))))

         (specify "Get a store arg - one byte memory loc"
                  (let ((*glx-memory* (vector #x102 #x05 #x08)))
                    (expect (glx-get-opcode-args #x102 glx-1) equals (list glx-3 (list (list #'glx-store-mem glx-8))))))

         (specify "Get a store arg - two byte memory loc"
                  (let ((*glx-memory* (vector #x102 #x06 #x16 #x08)))
                    (expect (glx-get-opcode-args #x102 glx-1) equals (list glx-4 (list (list #'glx-store-mem (glx-32 8 #x16)))))))

         (specify "Get a store arg - four byte memory loc"
                  (let ((*glx-memory* (vector #x102 #x07 #x00 #x03 #x00 #x08)))
                    (expect (glx-get-opcode-args #x102 glx-1) equals (list (glx-32 6) (list (list #'glx-store-mem (glx-32 8 0 3)))))))

         (specify "Get a store arg - store to stack"
                  (let ((*glx-memory* (vector #x102 #x08)))
                    (expect (glx-get-opcode-args #x102 glx-1) equals (list glx-2 (list (list #'glx-store-stack nil))))))

         (specify "Get a store arg - one byte local offset"
                  (let ((*glx-memory* (vector #x102 #x09 #x08)))
                    (expect (glx-get-opcode-args #x102 glx-1) equals (list glx-3 (list (list #'glx-store-local glx-8))))))

         (specify "Get a store arg - two byte local offset"
                  (let ((*glx-memory* (vector #x102 #x0a #x16 #x08)))
                    (expect (glx-get-opcode-args #x102 glx-1) equals (list glx-4 (list (list #'glx-store-local (glx-32 8 #x16)))))))

         (specify "Get a store arg - four byte local offset"
                  (let ((*glx-memory* (vector #x102 #x0b #x00 #x03 #x00 #x08)))
                    (expect (glx-get-opcode-args #x102 glx-1) equals (list (glx-32 6) (list (list #'glx-store-local (glx-32 8 0 3)))))))

         (specify "Get a store arg - one byte ram offset"
                  (let ((*glx-memory* (vector #x102 #x0d #x08))
                        (*glx-ram-start* glx-1))
                    (expect (glx-get-opcode-args #x102 glx-1) equals (list glx-3 (list (list #'glx-store-mem (glx-32 9)))))))

         (specify "Get a store arg - two byte ram offset"
                  (let ((*glx-memory* (vector #x102 #x0e #x16 #x08))
                        (*glx-ram-start* glx-1))
                    (expect (glx-get-opcode-args #x102 glx-1) equals (list glx-4 (list (list #'glx-store-mem (glx-32 9 #x16)))))))

         (specify "Get a store arg - four byte ram offset"
                  (let ((*glx-memory* (vector #x102 #x0f #x00 #x03 #x00 #x08))
                        (*glx-ram-start* glx-1))
                    (expect (glx-get-opcode-args #x102 glx-1) equals (list (glx-32 6) (list (list #'glx-store-mem (glx-32 9 0 3)))))))

         (specify "Unsupported addressing mode"
                  (let ((*glx-memory* (vector #x31 #x0c)))
                    (expect (glx-get-opcode-args #x31 glx-1) throws glx-exec-error)))

         (specify "Unsupported opcode"
                  (expect (glx-get-opcode-args #x164 glx-1) throws glx-exec-error))

         (specify "Can store 32 bits"
                  (let ((*glx-memory* (vector 0 0 0 0 0)))
                    (glx-store-mem glx-1 (glx-32 9 8 7 6) 4)
                    (expect *glx-memory* equals [0 6 7 8 9])))

         (specify "Can store 16 bits"
                  (let ((*glx-memory* (vector 0 0 0 0)))
                    (glx-store-mem glx-2 (glx-32 9 8 7 6) 2)
                    (expect *glx-memory* equals [0 0 8 9])))

         (specify "Can store 8 bits"
                  (let ((*glx-memory* (vector 0 0 0 0)))
                    (glx-store-mem glx-1 (glx-32 9 8 7 6) 1)
                    (expect *glx-memory* equals [0 9 0 0])))

         (specify "Store to address 0 does nothing"
                  (let ((*glx-memory* (vector 0 0 0 0)))
                    (glx-store-mem glx-0 (glx-32 9 8 7 6) 4)
                    (expect *glx-memory* equals [0 0 0 0]))))