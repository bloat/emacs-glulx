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

(ert-deftest test-glx-glk-call ()
  "test glx-glk-call"
  :tags '(glk)
  (let (fun-called
        (*glx-glk-functions* (make-hash-table))
        (*glx-stack* (list (list (list glx-2 glx-1)))))
    (puthash #x40 (list (lambda (arg1 arg2)
                          (setq fun-called t)
                          (should (equal arg1 glx-2))
                          (should (equal arg2 glx-1))
                          2)
                        #'identity #'identity) *glx-glk-functions*)
    (should (equal (glx-glk-call #x40 2) glx-2))
    (should fun-called)))

(ert-deftest test-void-return-value-translates-to-0 ()
  "test void return value translates to 0"
  :tags '(glk)
  (let ((*glx-glk-functions* (make-hash-table)))
    (puthash #x40 (list (lambda () nil))
             *glx-glk-functions*)
    (should (equal (glx-glk-call #x40 0) glx-0))))

(ert-deftest test-integer-return-value-is-converted-to-32-bits ()
  "test integer return value is converted to 32 bits"
  :tags '(glk)
  (let ((*glx-glk-functions* (make-hash-table)))
    (puthash #x40 (list (lambda () 5))
             *glx-glk-functions*)
    (should (equal (glx-glk-call #x40 0) glx-5))))

(ert-deftest store-arg-and-return-value ()
  "store arg and return value"
  :tags '(glk)
  (let ((*glx-glk-functions* (make-hash-table))
        (*glx-stack* (list (list (list glx-1))))
        (*glx-memory* (vector nil nil nil nil nil)))
    (puthash #x40 (list (lambda () (list '\(0\ 0\ 0\ 4\) '\(0\ 0\ 0\ 1\))) (list 1 #'glx-store-mem)) *glx-glk-functions*)
    (should (equal (glx-glk-call #x40 1) glx-4))
    (should (equal *glx-memory* [nil 0 0 0 1]))))

(ert-deftest a-store-value-which-is-a-list ()
  "a store value which is a list"
  :tags '(glk)
  (let (fun-called
        (*glx-glk-functions* (make-hash-table))
        (*glx-stack* (list (list (list glx-8)))))
    (puthash #x40 (list (lambda () (list 4 '(hello you)))
                        (list 1 (lambda (memptr result)
                                  (setq fun-called t)
                                  (should (equal memptr glx-8))
                                  (should (equal result '(hello you))))))
             *glx-glk-functions*)
    (should (equal (glx-glk-call #x40 1) glx-4))
    (should fun-called)))

(ert-deftest arity-mismatch ()
  "arity mismatch"
  :tags '(glk)
  (let ((*glx-glk-functions* (make-hash-table))
        (*glx-stack* (list (list (list glx-0))))
        (*glx-memory* [nil nil nil nil]))
    (puthash #x40 (list (lambda () glx-0)) *glx-glk-functions*)
    (should-error (glx-glk-call #x40 1) :type 'glx-glk-error)))

(ert-deftest glk-calls-that-need-new-ids ()
  "glk calls that need new ids"
  :tags '(glk)
  (let ((*glx-glk-functions* (make-hash-table))
        (*glx-stack* (list (list (list glx-2 glx-1))))
        (*glx-glk-id-gen* 0))
    (puthash #x40 (list (lambda (arg1 arg2)
                          (should (equal arg1 '\(0\ 0\ 0\ 1\)))
                          (should (equal arg2 '\(0\ 0\ 0\ 2\))))
                        'gen-id 'gen-id) *glx-glk-functions*)
    (glx-glk-call #x40 0)))

(ert-deftest glx-32->glk-opq-should-convert-0-to-nil ()
  "glx-32->glk-opq should convert 0 to nil"
  :tags '(glk)
  (should-not (glx-32->glk-opq glx-0)))

(ert-deftest glx-32->glk-opq-should-convert-other-values-to-a-symbol ()
  "glx-32->glk-opq should convert other values to a symbol"
  :tags '(glk)
  (should (equal (glx-32->glk-opq glx-4) '\(0\ 0\ 0\ 4\))))

(ert-deftest glx-glk-result->32-should-convert-nil-to-0 ()
  "glx-glk-result->32 should convert nil to 0"
  :tags '(glk)
  (should (equal (glx-glk-result->32 nil) glx-0)))

(ert-deftest glx-glk-result->32-should-convert-glk-opaques-to-32-bit-values ()
  "glx-glk-result->32 should convert glk opaques to 32 bit values"
  :tags '(glk)
  (should (equal (glx-glk-result->32 '\(0\ 0\ 0\ 5\)) glx-5)))

(ert-deftest storing-an-event-into-glulx-memory ()
  "storing an event into glulx memory"
  :tags '(glk)
  :expected-result :failed
  (let ((*glx-memory* (make-vector 33 0)))
    (glx-glk-store-event `(glk-evtype-lineinput \(0\ 0\ 0\ 1\) 16 0 ,(glx-32 16) "examine building") glx-1)
    (should (equal *glx-memory*
                   [0 0 0 0 3
                      0 0 0 1
                      0 0 0 16
                      0 0 0 0
                      101 120 97 109 105 110 101 32 98 117 105 108 100 105 110 103]))))

(ert-deftest storing-a-closed-memory-stream-into-glulx-memory ()
  "storing a closed memory stream into glulx memory"
  :tags '(glk)
  :expected-result :failed
  (let ((*glx-memory* (make-vector 14 0)))
    (glx-glk-store-closed-memory-stream glx-0 '(0 5 (0 0 0 9) "hello"))
    (should (equal *glx-memory*
                   [0 0 0 0
                      0 0 0 5
                      0 104 101 108 108 111]))))

(ert-deftest storing-a-closed-memory-stream-onto-the-stack ()
  "storing a closed memory stream onto the stack"
  :tags '(glk)
  (let ((*glx-memory* (make-vector 5 0))
        (*glx-stack* (list (list (list)))))
    (glx-glk-store-closed-memory-stream (glx-32 -1) '(0 5 nil (0 0 0 0) "hello"))
    (should (equal *glx-memory* [104 101 108 108 111]))
    (should (equal *glx-stack* (list (list (list glx-5 glx-0)))))))
