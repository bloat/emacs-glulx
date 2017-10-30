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

(ert-deftest push-something-onto-the-stack ()
  "Should be able to push something onto the stack"
  :tags '(stack)
  (let ((*glx-stack* nil))
    (glx-stack-push 56)
    (should (equal *glx-stack* '(56)))))

(ert-deftest pop-something-off-the-stack ()
  "Should be able to pop something off the stack"
  :tags '(stack)
  (let ((*glx-stack* '(56)))
    (should (= (glx-stack-pop) 56))
    (should-not *glx-stack*)))

(ert-deftest push-a-call-stub ()
  "Should be able to push a call stub"
  :tags '(stack)
  (let ((*glx-stack* nil)
        (*glx-pc* (glx-32 0 45)))
    (glx-push-call-stub 0 (glx-32 22 4 3 2))
    (should (equal *glx-stack* (list (list 0 (glx-32 22 4 3 2) *glx-pc*))))))

(ert-deftest build-a-new-call-frame---no-locals ()
  "Should be able to build a new call frame - no locals"
  :tags '(stack)
  (let ((*glx-stack* nil)
        (*glx-memory* [#xc0 0 0]))
    (glx-build-call-frame glx-0)
    (should (equal *glx-stack* '((() ()))))))

(ert-deftest build-a-new-call-frame---1-8-bit-local ()
  "Should be able to build a new call frame - 1 8 bit local"
  :tags '(stack)
  (let ((*glx-stack* ())
        (*glx-memory* [#xc0 1 1 0 0]))
    (glx-build-call-frame glx-0)
    (should (equal *glx-stack* (list (list nil (list (cons glx-0 glx-0))))))))

(ert-deftest build-a-new-call-frame---5-8-bit-locals ()
  "Should be able to build a new call frame - 5 8 bit locals"
  :tags '(stack)
  (let ((*glx-stack* nil)
        (*glx-memory* [#xc0 1 5 0 0]))
    (glx-build-call-frame glx-0)
    (should (equal *glx-stack* (list (list nil (list (cons glx-0 glx-0)
                                                     (cons glx-1 glx-0)
                                                     (cons glx-2 glx-0)
                                                     (cons glx-3 glx-0)
                                                     (cons glx-4 glx-0))))))))

(ert-deftest build-a-new-call-frame---1-16-bit-locals ()
  "Should be able to build a new call frame - 1 16 bit locals"
  :tags '(stack)
  (let ((*glx-stack* nil)
        (*glx-memory* [#xc0 2 1 0 0]))
    (glx-build-call-frame glx-0)
    (should (equal *glx-stack* (list (list nil (list (cons glx-0 glx-0))))))))

(ert-deftest build-a-new-call-frame---1-8-bit-local-2-16-bit-locals ()
  "Should be able to build a new call frame - 1 8 bit local, 2 16 bit locals"
  :tags '(stack)
  (let ((*glx-stack* nil)
        (*glx-memory* [#xc0 1 1 2 2 0 0]))
    (glx-build-call-frame glx-0)
    (should (equal *glx-stack* (list (list nil (list (cons glx-0 glx-0)
                                                            (cons glx-2 glx-0)
                                                            (cons glx-4 glx-0))))))))

(ert-deftest build-a-new-call-frame---1-8-_-2-32_-1-16_-3-8 ()
  "Should be able to build a new call frame - 1 8 , 2 32, 1 16, 3 8"
  :tags '(stack)
  (let ((*glx-stack* nil)
        (*glx-memory* [#xc0 1 1 4 2 2 1 1 3 0 0]))
    (glx-build-call-frame glx-0)
    (should (equal *glx-stack* (list (list nil (list (cons glx-0 glx-0)
                                                            (cons glx-4 glx-0)
                                                            (cons glx-8 glx-0)
                                                            (cons (glx-32 12) glx-0)
                                                            (cons (glx-32 14) glx-0)
                                                            (cons (glx-32 15) glx-0)
                                                            (cons (glx-32 16) glx-0))))))))

(ert-deftest call-a-stack-args-function-with-no-args ()
  "Should be able to call a stack args function with no args"
  :tags '(stack)
  (let ((*glx-stack* nil)
        (*glx-memory* [#xc0 0 0])
        (*glx-pc* (glx-32 400)))
    (glx-call-function glx-0 0 glx-0 nil)
    (should (equal *glx-stack* `(((,glx-0) ()) (0 ,glx-0 ,(glx-32 400)))))
    (should (equal *glx-pc* glx-3))))

(ert-deftest call-a-locals-args-function-with-no-args ()
  "Should be able to call a locals args function with no args"
  :tags '(stack)
  (let ((*glx-stack* nil)
        (*glx-memory* [#xc1 0 0])
        (*glx-pc* (glx-32 400)))
    (glx-call-function glx-0 0 glx-0 nil)
    (should (equal *glx-stack* `((() ()) (0 ,glx-0 ,(glx-32 400)))))
    (should (equal *glx-pc* glx-3))))

(ert-deftest call-a-stack-args-function-with-some-args ()
  "Should be able to call a stack args function with some args"
  :tags '(stack)
  (let ((*glx-stack* nil)
        (*glx-memory* [#xc0 0 0])
        (*glx-pc* (glx-32 400)))
    (glx-call-function glx-0 0 glx-0 (list glx-2 (glx-32 4 5 6 7)))
    (should (equal *glx-stack*
                   `(((,glx-2 ,glx-2 ,(glx-32 4 5 6 7)) ()) (0 ,glx-0 ,(glx-32 400)))))
    (should (equal *glx-pc* glx-3))))

(ert-deftest call-a-call-frame-locals-function-with-some-args ()
  "Should be able to call a call-frame locals function with some args"
  :tags '(stack)
  (let ((*glx-stack* ())
        (*glx-memory* [#xc1 1 1 2 1 4 1 0 0])
        (*glx-pc* (glx-32 400)))
    (glx-call-function glx-0 0 glx-0 (list (glx-32 1 2 3 4) (glx-32 1 2 3 4) (glx-32 1 2 3 4)))
    (should (equal *glx-stack* (list (list nil (list (cons glx-0 glx-1)
                                                            (cons glx-2 (glx-32 1 2))
                                                            (cons glx-4 (glx-32 1 2 3 4))))
                                            (list 0 glx-0 (glx-32 400)))))
    (should (equal *glx-pc* (glx-32 9)))))

(ert-deftest push-and-pop-a-value-into-the-current-call-frame ()
  "Should be able to push and pop a value into the current call frame"
  :tags '(stack)
  (let ((*glx-stack* nil)
        (*glx-memory* [#xc0 0 0]))
    (glx-build-call-frame glx-0)
    (glx-value-push (glx-32 45))
    (should (equal *glx-stack* `(((,(glx-32 45)) ()))))
    (should (equal (glx-stack-count) glx-1))
    (should (equal (glx-value-pop) (glx-32 45)))
    (should (equal *glx-stack* '((() ()))))))

(ert-deftest pop-past-the-current-call-frame ()
  "Should not be able to pop past the current call frame"
  :tags '(stack)
  (let ((*glx-stack* nil)
        (*glx-memory* [#xc0 0 0]))
    (glx-build-call-frame glx-0)
    (should (equal (glx-stack-count) glx-0))
    (should-error (glx-value-pop) :type 'glx-stack-error)))

(ert-deftest locals ()
  "Test getting locals"
  :tags '(stack)
  (let ((*glx-stack* ())
        (*glx-memory* [#xc0 1 1 0 0]))
    (glx-build-call-frame glx-0)
    (should (equal (glx-get-local-at-offset glx-0) glx-0))
    (should-error (glx-get-local-at-offset glx-1) :type 'glx-stack-error)))

(ert-deftest return-from-a-function ()
  "Should be able to return from a function"
  :tags '(stack)
  (let ((*glx-stack* (list (list (list nil nil)) (list 0 glx-0 glx-5))))
    (should (equal (glx-return-from-function) (list 0 glx-0 glx-5)))
    (should-not *glx-stack*)))

(ert-deftest peek-at-the-stack ()
  "Should be able to peek at the stack"
  :tags '(stack)
  (let ((*glx-stack* (list (list (list glx-0 glx-1 glx-2)))))
    (should (equal (glx-stack-peek 3) (list glx-0 glx-1 glx-2)))))
