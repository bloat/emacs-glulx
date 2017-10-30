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

(ert-deftest get-opcode-number ()
  "Get opcode number"
  :tags '(exec)
  (let ((*glx-memory* (vector #x45 #x9C #x34 #xC0 #x34 #x03 #xE9)))
    (should (equal (glx-get-opcode glx-0) (list glx-1 #x45)))
    (should (equal (glx-get-opcode glx-1) (list glx-3 #x1c34)))
    (should (equal (glx-get-opcode glx-3) (list (glx-32 7) #x3403e9)))))

(ert-deftest get-args-for-opcode-with-no-args ()
  "Get args for opcode with no args"
  :tags '(exec)
  (let ((*glx-memory* (vector #x00 #x45)))
    (should (equal (glx-get-opcode-args #x00 glx-1) (list glx-1 ())))))

(ert-deftest get-constant-zero-arg--one-arg-opcode ()
             "Get constant zero arg (one arg opcode)"
             :tags '(exec)
             (let ((*glx-memory* (vector #x31 #x00)))
               (should (equal (glx-get-opcode-args #x31 glx-1) (list glx-2 (list glx-0))))))

(ert-deftest get-constant-one-byte-arg--one-arg-opcode ()
             "Get constant one byte arg (one arg opcode)"
             :tags '(exec)
             (let ((*glx-memory* (vector #x31 #x01 #xde)))
               (should (equal (glx-get-opcode-args #x31 glx-1) (list glx-3 (list (glx-32 -34)))))))

(ert-deftest get-constant-two-byte-arg--one-arg-opcode ()
             "Get constant two byte arg (one arg opcode)"
             :tags '(exec)
             (let ((*glx-memory* (vector #x31 #x02 #xde #x4c)))
               (should (equal (glx-get-opcode-args #x31 glx-1) (list glx-4 (list (glx-32 -8628)))))))

(ert-deftest get-constant-4-byte-arg--one-arg-opcode ()
             "Get constant 4 byte arg (one arg opcode)"
             :tags '(exec)
             (let ((*glx-memory* (vector #x31 #x03 #x01 #x45 #x23 #xce)))
               (should (equal (glx-get-opcode-args #x31 glx-1) (list (glx-32 6) (list (glx-32 #x014523ce)))))))

(ert-deftest get-contents-of-1-byte-address-arg--one-arg-opcode ()
             "Get contents of 1 byte address arg (one arg opcode)"
             :tags '(exec)
             (let ((*glx-memory* (vector #x31 #x05 #x04 #x00 #x52 #x21 #x45 #xe1)))
               (should (equal (glx-get-opcode-args #x31 glx-1) (list glx-3 (list (glx-32 #xe1 #x45 #x21 #x52)))))))

(ert-deftest get-contents-of-2-byte-address-arg--one-arg-opcode ()
             "Get contents of 2 byte address arg (one arg opcode)"
             :tags '(exec)
             (let ((*glx-memory* (vector #x31 #x06 #x00 #x04 #x52 #x21 #x45 #xe1)))
               (should (equal (glx-get-opcode-args #x31 glx-1) (list glx-4 (list (glx-32 #xe1 #x45 #x21 #x52)))))))

(ert-deftest get-contents-of-4-byte-address-arg--one-arg-opcode ()
             "Get contents of 4 byte address arg (one arg opcode)"
             :tags '(exec)
             (let ((*glx-memory* (vector #x31 #x07 #x00 #x00 #x00 #x02)))
               (should (equal (glx-get-opcode-args #x31 glx-1) (list (glx-32 6) (list glx-2))))))

(ert-deftest get-an-arg-from-the-stack--one-arg-opcode ()
             "Get an arg from the stack (one arg opcode)"
             :tags '(exec)
             (let ((*glx-memory* (vector #x31 #x08))
                   (*glx-stack* `(((,glx-5) ()))))
               (should (equal (glx-get-opcode-args #x31 glx-1) (list glx-2 (list glx-5))))))

(ert-deftest get-an-arg-from-the-locals-list--one-byte-offset--one-arg-opcode ()
             "Get an arg from the locals list (one byte offset) (one arg opcode)"
             :tags '(exec)
             (let ((*glx-memory* (vector #x31 #x09 #x03))
                   (*glx-stack* (list (list nil (list (cons glx-3 glx-4))))))
               (should (equal (glx-get-opcode-args #x31 glx-1) (list glx-3 (list glx-4))))))

(ert-deftest get-an-arg-from-the-locals-list--two-byte-offset--one-arg-opcode ()
             "Get an arg from the locals list (two byte offset) (one arg opcode)"
             :tags '(exec)
             (let ((*glx-memory* (vector #x31 #x0a #x32 #x03))
                   (*glx-stack* (list (list nil (list (cons (glx-32 3 #x32) glx-5))))))
               (should (equal (glx-get-opcode-args #x31 glx-1) (list glx-4 (list glx-5))))))

(ert-deftest get-an-arg-from-the-locals-list--four-byte-offset--one-arg-opcode ()
             "Get an arg from the locals list (four byte offset) (one arg opcode)"
             :tags '(exec)
             (let ((*glx-memory* (vector #x31 #x0b #xc1 #xee #x32 #x03))
                   (*glx-stack* (list (list nil (list (cons (glx-32 3 #x32 #xee #xc1) glx-2))))))
               (should (equal (glx-get-opcode-args #x31 glx-1) (list (glx-32 6) (list glx-2))))))

(ert-deftest get-an-arg-from-ram--one-byte-offset--one-arg-opcode ()
             "Get an arg from RAM (one byte offset) (one arg opcode)"
             :tags '(exec)
             (let ((*glx-memory* (vector #x31 #x0d #x02 #xee #x32 #x03 #x45))
                   (*glx-ram-start* glx-1))
               (should (equal (glx-get-opcode-args #x31 glx-1) (list glx-3 (list (glx-32 #x45 3 #x32 #xee)))))))

(ert-deftest get-an-arg-from-ram--two-byte-offset--one-arg-opcode ()
             "Get an arg from RAM (two byte offset) (one arg opcode)"
             :tags '(exec)
             (let ((*glx-memory* (vector #x31 #x0e 0 #x03 #xee #x32 #x03 #x45))
                   (*glx-ram-start* glx-1))
               (should (equal (glx-get-opcode-args #x31 glx-1) (list glx-4 (list (glx-32 #x45 3 #x32 #xee)))))))

(ert-deftest get-an-arg-from-ram--four-byte-offset--one-arg-opcode ()
             "Get an arg from RAM (four byte offset) (one arg opcode)"
             :tags '(exec)
             (let ((*glx-memory* (vector #x31 #x0f 0 0 0 #x05 #xee #x32 #x03 #x45))
                   (*glx-ram-start* glx-1))
               (should (equal (glx-get-opcode-args #x31 glx-1) (list (glx-32 6) (list (glx-32 #x45 3 #x32 #xee)))))))

(ert-deftest get-a-few-different-kinds-of-args--three-arg-opcode ()
             "Get a few different kinds of args (three arg opcode)"
             :tags '(exec)
             (let ((*glx-memory* (vector #x23 #xe0 #x09 0 #x4 #x16 #x34 #x23 #x44 #x56))
                   (*glx-stack* (list (list nil (list (cons (glx-32 #x16) glx-4)))))
                   (*glx-ram-start* glx-2))
               (should (equal (glx-get-opcode-args #x25 glx-1) (list (glx-32 6) (list glx-0 (glx-32 #x56 #x44 #x23 #x34) glx-4))))))

(ert-deftest get-a-store-arg---store-to-nowhere ()
  "Get a store arg - store to nowhere"
  :tags '(exec)
  (let ((*glx-memory* (vector #x102 #x00)))
    (should (equal (glx-get-opcode-args #x102 glx-1) (list glx-2 (list (list #'glx-store-throw nil)))))))

(ert-deftest get-a-store-arg---one-byte-memory-loc ()
  "Get a store arg - one byte memory loc"
  :tags '(exec)
  (let ((*glx-memory* (vector #x102 #x05 #x08)))
    (should (equal (glx-get-opcode-args #x102 glx-1) (list glx-3 (list (list #'glx-store-mem glx-8)))))))

(ert-deftest get-a-store-arg---two-byte-memory-loc ()
  "Get a store arg - two byte memory loc"
  :tags '(exec)
  (let ((*glx-memory* (vector #x102 #x06 #x16 #x08)))
    (should (equal (glx-get-opcode-args #x102 glx-1) (list glx-4 (list (list #'glx-store-mem (glx-32 8 #x16))))))))

(ert-deftest get-a-store-arg---four-byte-memory-loc ()
  "Get a store arg - four byte memory loc"
  :tags '(exec)
  (let ((*glx-memory* (vector #x102 #x07 #x00 #x03 #x00 #x08)))
    (should (equal (glx-get-opcode-args #x102 glx-1) (list (glx-32 6) (list (list #'glx-store-mem (glx-32 8 0 3))))))))

(ert-deftest get-a-store-arg---store-to-stack ()
  "Get a store arg - store to stack"
  :tags '(exec)
  (let ((*glx-memory* (vector #x102 #x08)))
    (should (equal (glx-get-opcode-args #x102 glx-1) (list glx-2 (list (list #'glx-store-stack nil)))))))

(ert-deftest get-a-store-arg---one-byte-local-offset ()
  "Get a store arg - one byte local offset"
  :tags '(exec)
  (let ((*glx-memory* (vector #x102 #x09 #x08)))
    (should (equal (glx-get-opcode-args #x102 glx-1) (list glx-3 (list (list #'glx-store-local glx-8)))))))

(ert-deftest get-a-store-arg---two-byte-local-offset ()
  "Get a store arg - two byte local offset"
  :tags '(exec)
  (let ((*glx-memory* (vector #x102 #x0a #x16 #x08)))
    (should (equal (glx-get-opcode-args #x102 glx-1) (list glx-4 (list (list #'glx-store-local (glx-32 8 #x16))))))))

(ert-deftest get-a-store-arg---four-byte-local-offset ()
  "Get a store arg - four byte local offset"
  :tags '(exec)
  (let ((*glx-memory* (vector #x102 #x0b #x00 #x03 #x00 #x08)))
    (should (equal (glx-get-opcode-args #x102 glx-1) (list (glx-32 6) (list (list #'glx-store-local (glx-32 8 0 3))))))))

(ert-deftest get-a-store-arg---one-byte-ram-offset ()
  "Get a store arg - one byte ram offset"
  :tags '(exec)
  (let ((*glx-memory* (vector #x102 #x0d #x08))
        (*glx-ram-start* glx-1))
    (should (equal (glx-get-opcode-args #x102 glx-1) (list glx-3 (list (list #'glx-store-mem (glx-32 9))))))))

(ert-deftest get-a-store-arg---two-byte-ram-offset ()
  "Get a store arg - two byte ram offset"
  :tags '(exec)
  (let ((*glx-memory* (vector #x102 #x0e #x16 #x08))
        (*glx-ram-start* glx-1))
    (should (equal (glx-get-opcode-args #x102 glx-1) (list glx-4 (list (list #'glx-store-mem (glx-32 9 #x16))))))))

(ert-deftest get-a-store-arg---four-byte-ram-offset ()
  "Get a store arg - four byte ram offset"
  :tags '(exec)
  (let ((*glx-memory* (vector #x102 #x0f #x00 #x03 #x00 #x08))
        (*glx-ram-start* glx-1))
    (should (equal (glx-get-opcode-args #x102 glx-1) (list (glx-32 6) (list (list #'glx-store-mem (glx-32 9 0 3))))))))

(ert-deftest unsupported-addressing-mode ()
  "Unsupported addressing mode"
  :tags '(exec)
  (let ((*glx-memory* (vector #x31 #x0c)))
    (should-error (glx-get-opcode-args #x31 glx-1) :type 'glx-exec-error)))

(ert-deftest unsupported-opcode ()
  "Unsupported opcode"
  :tags '(exec)
  (should-error (glx-get-opcode-args #x164 glx-1) :type 'glx-exec-error))

(ert-deftest can-store-32-bits ()
  "Can store 32 bits"
  :tags '(exec)
  (let ((*glx-memory* (vector 0 0 0 0 0)))
    (glx-store-mem glx-1 (glx-32 9 8 7 6) 4)
    (should (equal *glx-memory* [0 6 7 8 9]))))

(ert-deftest can-store-16-bits ()
  "Can store 16 bits"
  :tags '(exec)
  (let ((*glx-memory* (vector 0 0 0 0)))
    (glx-store-mem glx-2 (glx-32 9 8 7 6) 2)
    (should (equal *glx-memory* [0 0 8 9]))))

(ert-deftest can-store-8-bits ()
  "Can store 8 bits"
  :tags '(exec)
  (let ((*glx-memory* (vector 0 0 0 0)))
    (glx-store-mem glx-1 (glx-32 9 8 7 6) 1)
    (should (equal *glx-memory* [0 9 0 0]))))

(ert-deftest store-to-address-0-does-nothing ()
  "Store to address 0 does nothing"
  :tags '(exec)
  (let ((*glx-memory* (vector 0 0 0 0)))
    (glx-store-mem glx-0 (glx-32 9 8 7 6) 4)
    (should (equal *glx-memory* [0 0 0 0]))))
