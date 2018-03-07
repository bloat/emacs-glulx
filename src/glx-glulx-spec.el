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

(ert-deftest should-be-able-to-get-a-byte-from-memory ()
  "Should be able to get a byte from memory"
  :tags '(glulx)
  (let ((*glx-memory* [45 245]))
    (should (equal (glx-memory-get-byte glx-0) (glx-32 45)))
    (should (equal (glx-memory-get-byte glx-1) (glx-32 245)))))

(ert-deftest should-be-able-to-get-a-signed-byte-value-from-memory ()
  "Should be able to get a signed byte value from memory"
  :tags '(glulx)
  (let ((*glx-memory* [#x45 #x90]))
    (should (equal (glx-memory-get-byte-signed glx-0) (glx-32 #x45)))
    (should (equal (glx-memory-get-byte-signed glx-1) (glx-32 -112)))))

(ert-deftest should-be-able-to-get-a-byte-value-from-memory-as-an-integer ()
  "Should be able to get a byte value from memory as an integer"
  :tags '(glulx)
  (let ((*glx-memory* [78 0]))
    (should (= (glx-memory-get-byte-int glx-0) 78))
    (should (= (glx-memory-get-byte-int glx-1) 0))))

(ert-deftest should-be-able-to-get-a-16-bit-value-from-memory ()
  "Should be able to get a 16 bit value from memory"
  :tags '(glulx)
  (let ((*glx-memory* [3 0 0 5 4 4]))
    (should (equal (glx-memory-get-16 glx-1) glx-0))
    (should (equal (glx-memory-get-16 glx-3) (glx-32 1284)))))

(ert-deftest should-be-able-to-get-a-signed-16-byte-value-from-memory ()
  "Should be able to get a signed 16 byte value from memory"
  :tags '(glulx)
  (let ((*glx-memory* [#x3d #x1c #xb9 #x9c]))
    (should (equal (glx-memory-get-16-signed glx-0) (glx-32 #x3d1c)))
    (should (equal (glx-memory-get-16-signed glx-2) (glx-32 -18020)))))

(ert-deftest should-be-able-to-get-a-32-bit-value-from-memory ()
  "Should be able to get a 32 bit value from memory"
  :tags '(glulx)
  (let ((*glx-memory* [3 0 0 5 4 4]))
    (should (equal (glx-memory-get-32 glx-1) (glx-32 4 5)))))

(ert-deftest should-be-able-to-set-a-32-bit-value-into-memory ()
  "Should be able to set a 32 bit value into memory"
  :tags '(glulx)
  (let ((*glx-memory* [3 0 0 5 4 4]))
    (glx-memory-set glx-1 glx-3 4)
    (should (equal *glx-memory* [3 0 0 0 3 4]))))

(ert-deftest should-be-able-to-set-a-16-bit-value-into-memory ()
  "Should be able to set a 16 bit value into memory"
  :tags '(glulx)
  (let ((*glx-memory* [3 7 8 5 4 4]))
    (glx-memory-set glx-1 (glx-32 4 5 6 7) 2)
    (should (equal *glx-memory* [3 5 4 5 4 4]))))

(ert-deftest should-be-able-to-set-an-8-bit-value-into-memory ()
  "Should be able to set an 8 bit value into memory"
  :tags '(glulx)
  (let ((*glx-memory* [3 7 8 5 4 4]))
    (glx-memory-set glx-1 (glx-32 4 5 6 7) 1)
    (should (equal *glx-memory* [3 4 8 5 4 4]))))

(ert-deftest linear-search---find-first-key ()
  "Linear search - find first key"
  :tags '(glulx)
  (let ((*glx-memory* [0 0 0 0]))
    (should (equal (glx-memory-linear-search glx-0 glx-4 glx-0 glx-4 glx-1 glx-0 glx-0) glx-0))))

(ert-deftest linear-search---find-a-subsequent-key ()
  "Linear search - find a subsequent key"
  :tags '(glulx)
  (let ((*glx-memory* [0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 3]))
    (should (equal (glx-memory-linear-search glx-2 glx-4 glx-0 glx-4 glx-4 glx-0 glx-0) glx-8))))

(ert-deftest linear-search---find-an-offset-subsequent-key ()
  "Linear search - find an offset subsequent key"
  :tags '(glulx)
  (let ((*glx-memory* [0 0 0 0 0 0 0 0 0 1 0 0 0 0 2 0 0 0 0 3]))
    (should (equal (glx-memory-linear-search glx-2 glx-4 glx-0 glx-5 glx-4 glx-1 glx-0) (glx-32 10)))))

(ert-deftest linear-search---key-not-found ()
  "Linear search - key not found"
  :tags '(glulx)
  (let ((*glx-memory* [0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 3]))
    (should (equal (glx-memory-linear-search glx-4 glx-4 glx-0 glx-4 glx-4 glx-0 glx-0) glx-0))))

(ert-deftest linear-search---unbounded-array ()
  "Linear search - unbounded array"
  :tags '(glulx)
  (let ((*glx-memory* [0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 3]))
    (should (equal (glx-memory-linear-search glx-2 glx-4 glx-0 glx-4 (glx-32 -1) glx-0 glx-0) glx-8))))

(ert-deftest linear-search---small-key ()
  "Linear search - small key"
  :tags '(glulx)
  (let ((*glx-memory* [0 0 0 1 0 2 0 3 0 4]))
    (should (equal (glx-memory-linear-search glx-2 glx-2 glx-0 glx-2 glx-5 glx-0 glx-0) glx-4))))

(ert-deftest linear-search---zero-terminated ()
  "Linear search - zero terminated"
  :tags '(glulx)
  (let ((*glx-memory* [0 0 0 1 0 0 0 0 0 0 0 2 0 0 0 3]))
    (should (equal (glx-memory-linear-search glx-3 glx-4 glx-0 glx-4 glx-4 glx-0 glx-2) glx-0))))

(ert-deftest linear-search---zero-terminated-searching-for-zero ()
  "Linear search - zero terminated, searching for zero"
  :tags '(glulx)
  (let ((*glx-memory* [0 0 0 1 0 0 0 0 0 0 0 2 0 0 0 3]))
    (should (equal (glx-memory-linear-search glx-0 glx-4 glx-0 glx-4 glx-4 glx-0 glx-2) glx-4))))

(ert-deftest linear-search---indirect-key ()
  "Linear search - indirect key"
  :tags '(glulx)
  (let ((*glx-memory* [0 0 0 0 1 0 0 0 0 0 0 0 0 0 2 0 0 0 0 3]))
    (should (equal (glx-memory-linear-search (glx-32 10) glx-5 glx-0 glx-5 glx-4 glx-0 glx-1) (glx-32 10)))))

(ert-deftest linear-search---return-index ()
  "Linear search - return index"
  :tags '(glulx)
  (let ((*glx-memory* [0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 3]))
    (should (equal (glx-memory-linear-search glx-2 glx-4 glx-0 glx-4 glx-4 glx-0 glx-4) glx-2))))

(ert-deftest linear-search---return-index-failure ()
  "Linear search - return index failure"
  :tags '(glulx)
  (let ((*glx-memory* [0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 3]))
    (should (equal (glx-memory-linear-search glx-5 glx-4 glx-0 glx-4 glx-4 glx-0 glx-4) (glx-32 -1)))))

(ert-deftest binary-search---find-middle-key ()
  "Binary search - find middle key"
  :tags '(glulx)
  (let ((*glx-memory* [0 0 0 3 0 0 0 0 0 0 0 4]))
    (should (equal (glx-memory-binary-search glx-0 glx-4 glx-0 glx-4 glx-1 glx-0 glx-0) glx-0))))

(ert-deftest binary-search---find-a-last-key ()
  "Binary search - find a last key"
  :tags '(glulx)
  (let ((*glx-memory* [0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 3]))
    (should (equal (glx-memory-binary-search glx-3 glx-4 glx-0 glx-4 glx-4 glx-0 glx-0) (glx-32 12)))))

(ert-deftest binary-search---find-a-first-key ()
  "Binary search - find a first key"
  :tags '(glulx)
  (let ((*glx-memory* [0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 3]))
    (should (equal (glx-memory-binary-search glx-0 glx-4 glx-0 glx-4 glx-4 glx-0 glx-0) glx-0))))

(ert-deftest linked-search---find-first-key ()
  "Linear search - find first key"
  :tags '(glulx)
  (let ((*glx-memory* [0 0 0 0 0 0 0 0]))
    (should (equal (glx-memory-linked-search glx-0 glx-4 glx-0 glx-0 glx-4 glx-0) glx-0))))

(ert-deftest linked-search---find-a-subsequent-key ()
  "Linear search - find a subsequent key"
  :tags '(glulx)
  (let ((*glx-memory* [0 0 0 0 0 0 0 16
                         0 0 0 1 0 0 0 0
                         0 0 0 2 0 0 0 24
                         0 0 0 3 0 0 0 8]))
    (should (equal (glx-memory-linked-search glx-2 glx-4 glx-0 glx-0 glx-4 glx-0) (glx-32 16)))))

(ert-deftest linked-search---find-an-offset-subsequent-key ()
  "Linked search - find an offset subsequent key"
  :tags '(glulx)
  (let ((*glx-memory* [0 0 0 0 0 0 0 0 27
                         0 0 0 0 1 0 0 0 18
                         0 0 0 0 2 0 0 0 0
                         0 0 0 0 3 0 0 0 9]))
    (should (equal (glx-memory-linked-search glx-2 glx-4 glx-0 glx-1 glx-5 glx-0) (glx-32 18)))))

(ert-deftest linked-search---key-not-found ()
  "Linked search - key not found"
  :tags '(glulx)
  (let ((*glx-memory* [0 0 0 0 0 0 0 16
                         0 0 0 1 0 0 0 0
                         0 0 0 2 0 0 0 24
                         0 0 0 3 0 0 0 8]))
    (should (equal (glx-memory-linked-search glx-4 glx-4 glx-0 glx-0 glx-4 glx-0) glx-0))))

(ert-deftest linked-search---small-key ()
  "Linked search - small key"
  :tags '(glulx)
  (let ((*glx-memory* [0 0 0 0 0 12
                         0 1 0 0 0 18 
                         0 2 0 0 0 6
                         0 3 0 0 0 24
                         0 4 0 0 0 0]))
    (should (equal (glx-memory-linked-search glx-4 glx-2 glx-0 glx-0 glx-2 glx-0) (glx-32 24)))))

(ert-deftest linked-search---indirect-key ()
  "Linked search - indirect key"
  :tags '(glulx)
  (let ((*glx-memory* [0 0 0 0 1 0 0 0 9
                         0 0 0 0 0 0 0 0 18
                         0 0 0 0 2 0 0 0 27
                         0 0 0 0 3 0 0 0 0]))
    (should (equal (glx-memory-linked-search (glx-32 27) glx-5 glx-0 glx-0 glx-5 glx-1) (glx-32 27)))))

(ert-deftest store-a-string-to-memory ()
  "Store a string to memory"
  :tags '(glulx)
  (let ((*glx-memory* [0 0 0 0 0 0]))
    (glx-memory-set-string glx-1 "Glulx")
    (should (equal *glx-memory* [0 71 108 117 108 120]))))

(ert-deftest save-undo ()
  "Save undo"
  :tags '(glulx)
  (let ((*glx-memory* (make-vector 4 0))
        (*glx-stack* (list (list nil (list (cons glx-0 glx-0)))))
        (*glx-pc* (list 0 3 4 5))
        (*glx-undo* nil))
    (glx-save-undo)
    (should (equal *glx-undo* '([0 0 0 0]
                                ((nil (((0 0 0 0) 0 0 0 0))))
                                (0 3 4 5))))
    (glx-value-push glx-4)
    (glx-memory-set glx-0 glx-5 4)
    (should (equal *glx-undo* '([0 0 0 0]
                                ((nil (((0 0 0 0) 0 0 0 0))))
                                (0 3 4 5))))))

(ert-deftest can-not-restore-undo-with-no-undo-saved ()
  "Can't restore undo with no undo saved"
  :tags '(glulx)
  (let ((*glx-undo* nil))
    (should (equal (glx-restore-undo) glx-1))))

(ert-deftest restore-undo ()
  "Restore undo"
  :tags '(glulx)
  (let ((*glx-undo* (list (make-vector 4 0)
                          (list (list nil (list (cons glx-0 glx-0))))
                          (list 0 3 4 5)))
        (*glx-memory* nil)
        (*glx-stack* nil)
        (*glx-pc* nil))
    (should (equal (glx-restore-undo) glx-0))
    (should (equal *glx-undo* nil))
    (should (equal *glx-memory* [0 0 0 0]))
    (should (equal *glx-stack* '((nil (((0 0 0 0) 0 0 0 0))))))
    (should (equal *glx-pc* '(0 3 4 5)))))

