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

(ert-deftest push-and-pop-something-onto-and-off-the-stack ()
  "Should be able to push something onto the stack and pop it off again"
  :tags '(stack)
  (let ((*glx-stack* ()))
    (glx-stack-push 56)
    (should (= (glx-stack-pop) 56))
    (should (not (glx-stack-pop)))))

(ert-deftest push-a-call-stub ()
  "Should be able to push a call stub"
  :tags '(stack)
  (let ((*glx-stack* ())
        (*glx-pc* (glx-32 0 45)))
    (glx-push-call-stub 0 (glx-32 22 4 3 2))
    (let ((stub (glx-stack-pop)))
      (should (= (glx-call-stub-dest-type stub) 0))
      (should (equal (glx-call-stub-dest-addr stub) (glx-32 22 4 3 2)))
      (should (equal (glx-call-stub-pc stub) *glx-pc*)))))

(ert-deftest build-a-new-call-frame---no-locals ()
  "Should be able to build a new call frame - no locals"
  :tags '(stack)
  (let ((*glx-stack* ())
        (*glx-memory* [#xc0 0 0]))
    (glx-build-call-frame glx-0)
    (should (equal (glx-stack-count) glx-0))
    (should (not (glx-get-all-local-offsets)))))

(ert-deftest build-a-new-call-frame---1-8-bit-local ()
  "Should be able to build a new call frame - 1 8 bit local"
  :tags '(stack)
  (let ((*glx-stack* ())
        (*glx-memory* [#xc0 1 1 0 0]))
    (glx-build-call-frame glx-0)
    (should (equal (glx-get-all-local-offsets) `(,glx-0)))
    (should (equal (glx-get-local-at-offset glx-0) glx-0))))

(ert-deftest build-a-new-call-frame---5-8-bit-locals ()
  "Should be able to build a new call frame - 5 8 bit locals"
  :tags '(stack)
  (let ((*glx-stack* ())
        (*glx-memory* [#xc0 1 5 0 0]))
    (glx-build-call-frame glx-0)
    (should (= (length (glx-get-all-local-offsets)) 5))
    (should (equal (glx-get-local-at-offset glx-0) glx-0))
    (should (equal (glx-get-local-at-offset glx-1) glx-0))
    (should (equal (glx-get-local-at-offset glx-2) glx-0))
    (should (equal (glx-get-local-at-offset glx-3) glx-0))
    (should (equal (glx-get-local-at-offset glx-4) glx-0))))

(ert-deftest build-a-new-call-frame---1-16-bit-locals ()
  "Should be able to build a new call frame - 1 16 bit locals"
  :tags '(stack)
  (let ((*glx-stack* ())
        (*glx-memory* [#xc0 2 1 0 0]))
    (glx-build-call-frame glx-0)
    (should (= (length (glx-get-all-local-offsets)) 1))
    (should (equal (glx-get-local-at-offset glx-0) glx-0))))

(ert-deftest build-a-new-call-frame---1-8-bit-local-2-16-bit-locals ()
  "Should be able to build a new call frame - 1 8 bit local, 2 16 bit locals"
  :tags '(stack)
  (let ((*glx-stack* ())
        (*glx-memory* [#xc0 1 1 2 2 0 0]))
    (glx-build-call-frame glx-0)
    (should (= (length (glx-get-all-local-offsets)) 3))
    (should (equal (glx-get-local-at-offset glx-0) glx-0))
    (should (equal (glx-get-local-at-offset glx-2) glx-0))
    (should (equal (glx-get-local-at-offset glx-4) glx-0))))

(ert-deftest build-a-new-call-frame---1-8-_-2-32_-1-16_-3-8 ()
  "Should be able to build a new call frame - 1 8 , 2 32, 1 16, 3 8"
  :tags '(stack)
  (let ((*glx-stack* ())
        (*glx-memory* [#xc0 1 1 4 2 2 1 1 3 0 0]))
    (glx-build-call-frame glx-0)
    (should (= (length (glx-get-all-local-offsets)) 7))
    (should (equal (glx-get-local-at-offset glx-0) glx-0))
    (should (equal (glx-get-local-at-offset glx-4) glx-0))
    (should (equal (glx-get-local-at-offset glx-8) glx-0))
    (should (equal (glx-get-local-at-offset (glx-32 12)) glx-0))
    (should (equal (glx-get-local-at-offset (glx-32 14)) glx-0))
    (should (equal (glx-get-local-at-offset (glx-32 15)) glx-0))
    (should (equal (glx-get-local-at-offset (glx-32 16)) glx-0))))

(ert-deftest call-a-stack-args-function-with-no-args ()
  "Should be able to call a stack args function with no args"
  :tags '(stack)
  (let ((*glx-stack* ())
        (*glx-memory* [#xc0 0 0])
        (*glx-pc* (glx-32 400))
        (*glx-accelerated-functions* (make-hash-table)))
    (glx-call-function glx-0 0 glx-0 nil)
    (should (zerop (length (glx-get-all-local-offsets))))
    (should (equal (glx-stack-count) glx-1))
    (should (equal (glx-value-pop) glx-0))
    (glx-stack-pop) ; discard call frame
    (let ((stub (glx-stack-pop)))
      (should (= (glx-call-stub-dest-type stub) 0))
      (should (equal (glx-call-stub-dest-addr stub) glx-0))
      (should (equal (glx-call-stub-pc stub) (glx-32 400))))
    (should (equal *glx-pc* glx-3))))

(ert-deftest call-a-locals-args-function-with-no-args ()
  "Should be able to call a locals args function with no args"
  :tags '(stack)
  (let ((*glx-stack* ())
        (*glx-memory* [#xc1 0 0])
        (*glx-pc* (glx-32 400))
        (*glx-accelerated-functions* (make-hash-table)))
    (glx-call-function glx-0 0 glx-0 nil)
    (should (zerop (length (glx-get-all-local-offsets))))
    (should (equal (glx-stack-count) glx-0))))

(ert-deftest call-a-stack-args-function-with-some-args ()
  "Should be able to call a stack args function with some args"
  :tags '(stack)
  (let ((*glx-stack* ())
        (*glx-memory* [#xc0 0 0])
        (*glx-pc* (glx-32 400))
        (*glx-accelerated-functions* (make-hash-table)))
    (glx-call-function glx-0 0 glx-0 (list glx-2 (glx-32 4 5 6 7)))
    (should (equal (glx-stack-count) glx-3))
    (should (equal (glx-stack-peek 3) `(,glx-2 ,glx-2 ,(glx-32 4 5 6 7))))))

(ert-deftest call-a-call-frame-locals-function-with-some-args ()
  "Should be able to call a call-frame locals function with some args"
  :tags '(stack)
  (let ((*glx-stack* ())
        (*glx-memory* [#xc1 1 1 2 1 4 1 0 0])
        (*glx-pc* (glx-32 400))
        (*glx-accelerated-functions* (make-hash-table)))
    (glx-call-function glx-0 0 glx-0 (list (glx-32 1 2 3 4) (glx-32 1 2 3 4) (glx-32 1 2 3 4)))
    (should (equal (glx-stack-count) glx-0))
    (should (= (length (glx-get-all-local-offsets)) 3))
    (should (equal (glx-get-local-at-offset glx-0) glx-1))
    (should (equal (glx-get-local-at-offset glx-2) (glx-32 1 2)))
    (should (equal (glx-get-local-at-offset glx-4) (glx-32 1 2 3 4)))))

(ert-deftest call-an-accelerated-function ()
  "Should be able to call an accelerated function"
  :tags '(stack)
  (let ((*glx-accelerated-functions* (make-hash-table))
        (*glx-stack* ())
        (*glx-memory* (vector #xc1 1 1 2 1 4 1 0 0))
        (*glx-pc* (glx-32 400)))
    (let (accelerated-args)
      (cl-letf (((symbol-function 'accelerated-function) (lambda (&rest args) (setq accelerated-args args) glx-5)))
        (puthash glx-0 (list #'accelerated-function 2) *glx-accelerated-functions*)
        (glx-call-function glx-0 1 glx-4 (list glx-3 glx-5)))
      (should (equal accelerated-args (list glx-3 glx-5)))
      (should (equal *glx-memory* [#xc1 1 1 2 0 0 0 5 0])))))

(ert-deftest call-an-accelerated-function-with-too-many-args ()
  "Should be able to call an accelerated function - extra args should be ignored"
  :tags '(stack)
  (let ((*glx-accelerated-functions* (make-hash-table))
        (*glx-stack* ())
        (*glx-memory* (vector #xc1 1 1 2 1 4 1 0 0))
        (*glx-pc* (glx-32 400)))
    (let (accelerated-args)
      (cl-letf (((symbol-function 'accelerated-function) (lambda (&rest args) (setq accelerated-args args) glx-5)))
        (puthash glx-0 (list #'accelerated-function 2) *glx-accelerated-functions*)
        (glx-call-function glx-0 1 glx-4 (list glx-3 glx-5 glx-2 glx-4)))
      (should (equal accelerated-args (list glx-3 glx-5)))
      (should (equal *glx-memory* [#xc1 1 1 2 0 0 0 5 0])))))

(ert-deftest call-an-accelerated-function-with-too-few-args ()
  "Should be able to call an accelerated function - should add zero args"
  :tags '(stack)
  (let ((*glx-accelerated-functions* (make-hash-table))
        (*glx-stack* ())
        (*glx-memory* (vector #xc1 1 1 2 1 4 1 0 0))
        (*glx-pc* (glx-32 400)))
    (let (accelerated-args)
      (cl-letf (((symbol-function 'accelerated-function) (lambda (&rest args) (setq accelerated-args args) glx-5)))
        (puthash glx-0 (list #'accelerated-function 2) *glx-accelerated-functions*)
        (glx-call-function glx-0 1 glx-4 (list glx-3)))
      (should (equal accelerated-args (list glx-3 glx-0)))
      (should (equal *glx-memory* [#xc1 1 1 2 0 0 0 5 0])))))

(ert-deftest return-from-function-ignore-result ()
  "return from function - ignore result"
  :tags '(stack)

  (let ((*glx-stack* (list (list) (list 0 glx-0 glx-4)))
        (*glx-pc* glx-5))
    (glx-return-from-function glx-3)
    (should (equal *glx-stack* nil))
    (should (equal *glx-pc* glx-4))))

(ert-deftest return-from-function-result-on-stack ()
  "return from function - push result onto stack"
  :tags '(stack)

  (let ((*glx-stack* (list (list) (list 3 glx-0 glx-4) (list (list) (list))))
        (*glx-pc* glx-5))
    (glx-return-from-function glx-3)
    (should (equal *glx-stack* (list (list (list glx-3) (list)))))
    (should (equal *glx-pc* glx-4))))

(ert-deftest return-from-function-result-in-locals ()
  "return from function - put result into locals"
  :tags '(stack)

  (let ((*glx-stack* (list (list) (list 2 glx-0 glx-4) (list (list) (list (cons glx-0 glx-0)))))
        (*glx-pc* glx-5))
    (glx-return-from-function glx-3)
    (should (equal *glx-stack* (list (list (list) (list (cons glx-0 glx-3))))))
    (should (equal *glx-pc* glx-4))))

(ert-deftest return-from-function-result-in-memory ()
  "return from function - put result into memory"
  :tags '(stack)

  (let ((*glx-stack* (list (list) (list 1 glx-1 glx-4)))
        (*glx-pc* glx-5)
        (*glx-memory* (vector 1 1 1 1 1)))
    (glx-return-from-function glx-3)
    (should (equal *glx-stack* nil))
    (should (equal *glx-pc* glx-4))
    (should (equal *glx-memory* [1 0 0 0 3]))))

(ert-deftest push-and-pop-a-value-into-the-current-call-frame ()
  "Should be able to push and pop a value into the current call frame"
  :tags '(stack)
  (let ((*glx-stack* ()))
    (glx-push-new-call-frame ())
    (glx-value-push (glx-32 45))
    (should (equal (glx-stack-count) glx-1))
    (should (equal (glx-value-pop) (glx-32 45)))
    (should (equal (glx-stack-count) glx-0))))

(ert-deftest pop-past-the-current-call-frame ()
  "Should not be able to pop past the current call frame"
  :tags '(stack)
  (let ((*glx-stack* ()))
    (glx-push-new-call-frame ())
    (should (equal (glx-stack-count) glx-0))
    (should-error (glx-value-pop) :type 'glx-stack-error)))

(ert-deftest catch-tokens-do-not-count-towards-stack-length ()
  "catch tokens do not count towards stack length"
  :tags '(stack)

  (let ((*glx-stack* ()))
    (glx-push-new-call-frame ())
    (glx-value-push glx-0)
    (glx-catch-push glx-2)
    (glx-value-push glx-1)
    (should (equal (glx-stack-count) glx-2))))

(ert-deftest catch-tokens-are-discarded-not-popped ()
  "catch tokens are discarded, not popped"
  :tags '(stack)

  (let ((*glx-stack* ()))
    (glx-push-new-call-frame ())
    (glx-value-push glx-0)
    (glx-catch-push glx-2)
    (glx-value-push glx-1)
    (should (equal (glx-value-pop) glx-1))
    (should (equal (glx-value-pop) glx-0))
    (should (equal (glx-stack-count) glx-0))))

(ert-deftest locals ()
  "Test getting locals"
  :tags '(stack)
  
  (let ((*glx-stack* ())
        (*glx-memory* [#xc0 1 1 0 0]))
    (glx-build-call-frame glx-0)
    (should (equal (glx-get-local-at-offset glx-0) glx-0))
    (should-error (glx-get-local-at-offset glx-1) :type 'glx-stack-error)))

(ert-deftest peek-at-the-stack ()
  "Should be able to peek at the stack"
  :tags '(stack)

  (let ((*glx-stack* ()))
    (glx-push-new-call-frame ())
    (glx-value-push glx-2)
    (glx-value-push glx-1)
    (glx-value-push glx-0)
    (should (equal (glx-stack-peek 3) (list glx-0 glx-1 glx-2)))
    (should (equal (glx-stack-peek 2) (list glx-0 glx-1))))

  (let ((*glx*-stack* ()))
    (glx-push-new-call-frame ())
    (glx-value-push glx-4)
    (glx-value-push glx-3)
    (glx-catch-push glx-2) ; catch token should not be peekable
    (glx-value-push glx-0)
    (should (equal (glx-stack-peek 2) (list glx-0 glx-3)))))

(ert-deftest tailcall ()
  "Manipulate the stack to implement a tail call"
  :tags 'stack

  (let ((*glx-stack* ())
        (*glx-pc* glx-8)
        (*glx-memory* [#xc0 0 0])
        (*glx-accelerated-functions* (make-hash-table)))
    (glx-push-call-stub 0 glx-1)
    (glx-push-new-call-frame (list (cons glx-0 glx-0)))
    (glx-value-push glx-1)

    (glx-tailcall-function glx-0 nil)
    
    ;; new call frame stack contains only the arg count for the newly called function
    (should (equal (glx-value-pop) glx-0))
    (should (equal (glx-stack-count) glx-0))
    ;; No locals in the newly called function
    (should (not (glx-get-all-local-offsets)))
    
    (should (equal *glx-pc* glx-3)) ; first instruction of the newly called function

    ;; Discard call frame and check call stub, should be
    ;; as it was before tail call.
    (glx-stack-pop)
    (let ((stub (glx-stack-pop)))
      (should (= (glx-call-stub-dest-type stub) 0))
      (should (equal (glx-call-stub-dest-addr stub) glx-1))
      (should (equal (glx-call-stub-pc stub) glx-8)))))

(ert-deftest tailcall-accelerated ()
  "tailcall an accelerated function"
  :tags 'stack

  (let ((*glx-stack* ())
        (*glx-pc* glx-8)
        (*glx-memory* (vector #xc0 0 0 1 1 1 1))
        (*glx-accelerated-functions* (make-hash-table))
        (accelerated-args))
    (cl-letf (((symbol-function 'accelerated-function) (lambda (&rest args) (setq accelerated-args args) glx-5)))
      (puthash glx-0 (list #'accelerated-function 2) *glx-accelerated-functions*)
      (glx-push-call-stub 1 glx-3)
      (glx-push-new-call-frame (list (cons glx-0 glx-0)))
      (glx-value-push glx-1)
      (glx-tailcall-function glx-0 (list glx-3 glx-5)))
    (should (equal accelerated-args (list glx-3 glx-5)))
    (should (equal *glx-memory* [#xc0 0 0 0 0 0 5]))))

(ert-deftest unwind ()
  "Unwind the stack to help implement a throw"
  :tags '(stack)

  ;; Nothing on the stack
  (let ((*glx-stack* ()))
    (should-error (glx-stack-unwind glx-0) :type 'glx-stack-error))

  ;; An empty call frame, and a call stub
  (let ((*glx-stack* ()))
    (glx-push-call-stub 0 glx-0)
    (glx-push-new-call-frame ())
    (should-error (glx-stack-unwind glx-0) :type 'glx-stack-error))

  ;; A call frame with a matching token
  (let ((*glx-stack* ()))
    (glx-push-call-stub 0 glx-0)
    (glx-push-new-call-frame ())
    (glx-value-push glx-1)
    (glx-catch-push glx-0)
    (should (glx-stack-unwind glx-0))
    (should (equal (glx-stack-count) glx-1))
    (should (equal (glx-value-pop) glx-1)))

  ;; A token can only be used once
  (let ((*glx-stack* ()))
    (glx-push-call-stub 0 glx-0)
    (glx-push-new-call-frame ())
    (glx-catch-push glx-0)
    (should (glx-stack-unwind glx-0))
    (should-error (glx-stack-unwind glx-0)))

  ;; Ignores non-matching tokens
  (let ((*glx-stack* ()))
    (glx-push-call-stub 0 glx-0)
    (glx-push-new-call-frame ())
    (glx-value-push glx-2)
    (glx-catch-push glx-1)

    (glx-push-call-stub 0 glx-3)
    (glx-push-new-call-frame ())
    (glx-value-push glx-3)
    (glx-catch-push glx-2)
    (glx-value-push glx-4)
    
    (should (glx-stack-unwind glx-1))
    (should (equal (glx-value-pop) glx-2))))
