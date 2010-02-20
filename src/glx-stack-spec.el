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

(context "The stack: "
         (tag stack)

         (specify "Should be able to push something onto the stack"
                  (let ((*glx-stack* nil))
                    (glx-stack-push 56)
                    (expect *glx-stack* equals '(56))))

         (specify "Should be able to pop something off the stack"
                  (let ((*glx-stack* '(56)))
                    (expect (glx-stack-pop) equals 56)
                    (expect (not *glx-stack*))))

         (specify "Should be able to push a call stub"
                  (let ((*glx-stack* nil)
                        (*glx-pc* (glx-32 0 45)))
                    (glx-push-call-stub 0 (glx-32 22 4 3 2))
                    (expect *glx-stack* equals (list (list 0 (glx-32 22 4 3 2) *glx-pc*)))))

         (specify "Should be able to build a new call frame - no locals"
                  (let ((*glx-stack* nil)
                        (*glx-memory* [#xc0 0 0]))
                    (glx-build-call-frame glx-0)
                    (expect *glx-stack* equals '((() ())))))

         (specify "Should be able to build a new call frame - 1 8 bit local"
                  (let ((*glx-stack* ())
                        (*glx-memory* [#xc0 1 1 0 0]))
                    (glx-build-call-frame glx-0)
                    (expect *glx-stack* equals (list (list nil (list (cons glx-0 glx-0)))))))

         (specify "Should be able to build a new call frame - 5 8 bit locals"
                  (let ((*glx-stack* nil)
                        (*glx-memory* [#xc0 1 5 0 0]))
                    (glx-build-call-frame glx-0)
                    (expect *glx-stack* equals (list (list nil (list (cons glx-0 glx-0)
                                                                     (cons glx-1 glx-0)
                                                                     (cons glx-2 glx-0)
                                                                     (cons glx-3 glx-0)
                                                                     (cons glx-4 glx-0)))))))

         (specify "Should be able to build a new call frame - 1 16 bit locals"
                  (let ((*glx-stack* nil)
                        (*glx-memory* [#xc0 2 1 0 0]))
                    (glx-build-call-frame glx-0)
                    (expect *glx-stack* equals (list (list nil (list (cons glx-0 glx-0)))))))

         (specify "Should be able to build a new call frame - 1 8 bit local, 2 16 bit locals"
                  (let ((*glx-stack* nil)
                        (*glx-memory* [#xc0 1 1 2 2 0 0]))
                    (glx-build-call-frame glx-0)
                    (expect *glx-stack* equals (list (list nil (list (cons glx-0 glx-0)
                                                                     (cons glx-2 glx-0)
                                                                     (cons glx-4 glx-0)))))))

         (specify "Should be able to build a new call frame - 1 8 , 2 32, 1 16, 3 8 "
                  (let ((*glx-stack* nil)
                        (*glx-memory* [#xc0 1 1 4 2 2 1 1 3 0 0]))
                    (glx-build-call-frame glx-0)
                    (expect *glx-stack* equals (list (list nil (list (cons glx-0 glx-0)
                                                                     (cons glx-4 glx-0)
                                                                     (cons glx-8 glx-0)
                                                                     (cons (glx-32 12) glx-0)
                                                                     (cons (glx-32 14) glx-0)
                                                                     (cons (glx-32 15) glx-0)
                                                                     (cons (glx-32 16) glx-0)))))))

         (specify "Should be able to call a stack args function with no args"
                  (let ((*glx-stack* nil)
                        (*glx-memory* [#xc0 0 0])
                        (*glx-pc* (glx-32 400)))
                    (glx-call-function glx-0 0 glx-0 nil)
                    (expect *glx-stack* equals `(((,glx-0) ()) (0 ,glx-0 ,(glx-32 400))))
                    (expect *glx-pc* equals glx-3)))

         (specify "Should be able to call a locals args function with no args"
                  (let ((*glx-stack* nil)
                        (*glx-memory* [#xc1 0 0])
                        (*glx-pc* (glx-32 400)))
                    (glx-call-function glx-0 0 glx-0 nil)
                    (expect *glx-stack* equals `((() ()) (0 ,glx-0 ,(glx-32 400))))
                    (expect *glx-pc* equals glx-3)))

         (specify "Should be able to call a stack args function with some args"
                  (let ((*glx-stack* nil)
                        (*glx-memory* [#xc0 0 0])
                        (*glx-pc* (glx-32 400)))
                    (glx-call-function glx-0 0 glx-0 (list glx-2 (glx-32 4 5 6 7)))
                    (expect *glx-stack* equals
                            `(((,glx-2 ,glx-2 ,(glx-32 4 5 6 7)) ()) (0 ,glx-0 ,(glx-32 400))))
                    (expect *glx-pc* equals glx-3)))

         (specify "Should be able to call a call-frame locals function with some args"
                  (let ((*glx-stack* ())
                        (*glx-memory* [#xc1 1 1 2 1 4 1 0 0])
                        (*glx-pc* (glx-32 400)))
                    (glx-call-function glx-0 0 glx-0 (list (glx-32 1 2 3 4) (glx-32 1 2 3 4) (glx-32 1 2 3 4)))
                    (expect *glx-stack* equals (list (list nil (list (cons glx-0 glx-1)
                                                                     (cons glx-2 (glx-32 1 2))
                                                                     (cons glx-4 (glx-32 1 2 3 4))))
                                                     (list 0 glx-0 (glx-32 400))))
                    (expect *glx-pc* equals (glx-32 9))))

         (specify "Should be able to push and pop a value into the current call frame"
                  (let ((*glx-stack* nil)
                        (*glx-memory* [#xc0 0 0]))
                    (glx-build-call-frame glx-0)
                    (glx-value-push (glx-32 45))
                    (expect *glx-stack* equals `(((,(glx-32 45)) ())))
                    (expect (glx-value-pop) equals (glx-32 45))
                    (expect *glx-stack* equals '((() ())))))

         (specify "Should not be able to pop past the current call frame"
                  (let ((*glx-stack* nil)
                        (*glx-memory* [#xc0 0 0]))
                    (glx-build-call-frame glx-0)
                    (expect (glx-value-pop) throws 'glx-stack-error)))

         (specify "Test getting locals"
                  (let ((*glx-stack* ())
                        (*glx-memory* [#xc0 1 1 0 0]))
                    (glx-build-call-frame glx-0)
                    (expect (glx-get-local-at-offset glx-0) equals glx-0)
                    (expect (glx-get-local-at-offset glx-1) throws glx-stack-error)))

         (specify "Should be able to return from a function"
                  (let ((*glx-stack* (list (list (list nil nil)) (list 0 glx-0 glx-5))))
                    (expect (glx-return-from-function) equals (list 0 glx-0 glx-5))
                    (expect *glx-stack* equals nil)))

         (specify "Should be able to peek at the stack"
                  (let ((*glx-stack* (list (list (list glx-0 glx-1 glx-2)))))
                    (expect (glx-stack-peek 3) equals (list glx-0 glx-1 glx-2)))))
