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

(ert-deftest compile-the-last-function ()
  "Compile the last function"
  :tags '(compile)
  (let ((compiled-function-check nil))
    (cl-letf (((symbol-function 'glx-compile-function) (lambda (ptr) (setq compiled-function-check ptr) nil)))
      (glx-compile-rom 'a-ptr))
    (should (equal compiled-function-check 'a-ptr))))

(ert-deftest compile-the-more-functions ()
  "Compile the more functions"
  :tags '(compile)
  (let ((compiled-functions-check (list)))
    (cl-letf (((symbol-function 'glx-compile-function) (lambda (ptr)
                                                         (push ptr compiled-functions-check)
                                                         (when
                                                             (eq ptr 'a-ptr) (list 'a-ptr2 'a-ptr3)))))
      (glx-compile-rom 'a-ptr))
    (should (equal compiled-functions-check '(a-ptr3 a-ptr2 a-ptr)))))

(ert-deftest do-not-recompile-functions ()
  "Don't recompile functions"
  :tags '(compile)
  (let ((compiled-functions-check (list)))
    (cl-letf (((symbol-function 'glx-compile-function) (lambda (ptr)
                                                         (push ptr compiled-functions-check)
                                                         (should (< (length compiled-functions-check) 2))
                                                         nil)))
      (glx-compile-rom 'a-ptr))
    (should (equal compiled-functions-check '(a-ptr)))))

(ert-deftest compile-constant-0-arg ()
  "Compile constant 0 arg"
  :tags '(compile)
  (let ((compiled-arg (car (glx-compile-load-arg 0 glx-0))))
    (should (equal (funcall (car compiled-arg) (cadr compiled-arg)) glx-0))))

(ert-deftest compile-constant-1-byte-arg ()
  "Compile constant 1 byte arg"
  :tags '(compile)
  (let ((*glx-memory* (vector #x31 #x01 #x4e)))
    (let ((compiled-arg (car (glx-compile-load-arg 1 glx-2))))
      (should (equal (funcall (car compiled-arg) (cadr compiled-arg)) (glx-32 #x4e))))))

(ert-deftest compile-constant-2-byte-arg ()
  "Compile constant 2 byte arg"
  :tags '(compile)
  (let ((*glx-memory* (vector #x31 #x01 #x4e #x32)))
    (let ((compiled-arg (car (glx-compile-load-arg 2 glx-2))))
      (should (equal (funcall (car compiled-arg) (cadr compiled-arg)) (glx-32 #x32 #x4e))))))

(ert-deftest compile-constant-4-byte-arg ()
  "Compile constant 4 byte arg"
  :tags '(compile)
  (let ((*glx-memory* (vector #x31 #x01 #x4e #x32 #x00 #xff)))
    (let ((compiled-arg (car (glx-compile-load-arg 3 glx-2))))
      (should (equal (funcall (car compiled-arg) (cadr compiled-arg)) (glx-32 #xff #x00 #x32 #x4e))))))

(ert-deftest compile-get-contents-of-1-byte-address-arg ()
  "Compile get contents of 1 byte address arg"
  :tags '(compile)
  (let ((*glx-memory* (vector #x31 #x05 #x04 #x00 #x52 #x21 #x45 #xe1)))
    (let ((compiled-arg (car (glx-compile-load-arg 5 glx-2))))
      (should (equal (funcall (car compiled-arg) (cadr compiled-arg)) (glx-32 #xe1 #x45 #x21 #x52))))))

(ert-deftest compile-get-contents-of-2-byte-address-arg ()
  "Compile get contents of 2 byte address arg"
  :tags '(compile)
  (let ((*glx-memory* (vector #x31 #x06 #x00 #x04 #x52 #x21 #x45 #xe1)))
    (let ((compiled-arg (car (glx-compile-load-arg 6 glx-2))))
      (should (equal (funcall (car compiled-arg) (cadr compiled-arg)) (glx-32 #xe1 #x45 #x21 #x52))))))

(ert-deftest compile-get-contents-of-4-byte-address-arg ()
  "Compile get contents of 4 byte address arg"
  :tags '(compile)
  (let ((*glx-memory* (vector #x31 #x07 #x00 #x00 #x00 #x02)))
    (let ((compiled-arg (car (glx-compile-load-arg 7 glx-2))))
      (should (equal (funcall (car compiled-arg) (cadr compiled-arg)) glx-2)))))

(ert-deftest compile-stack-arg ()
  "Compile stack arg"
  :tags '(compile)
  (let ((compiled-arg (car (glx-compile-load-arg 8 nil))))
    (let ((*glx-stack* `(((,glx-5) ()))))
      (should (equal (funcall (car compiled-arg) (cadr compiled-arg)) glx-5)))))

(ert-deftest compile-locals-arg-one-byte-offset ()
  "Compile locals arg (one byte offset)"
  :tags '(compile)
  (let ((*glx-memory* (vector #x31 #x09 #x03)))
    (let ((compiled-arg (car (glx-compile-load-arg 9 glx-2))))
      (let ((*glx-stack* (list (list nil (list (cons glx-3 glx-4))))))
        (should (equal (funcall (car compiled-arg) (cadr compiled-arg)) glx-4))))))

(ert-deftest compile-locals-arg-two-byte-offset ()
  "Compile locals arg (two byte offset)"
  :tags '(compile)
  (let ((*glx-memory* (vector #x31 #x0a #x32 #x03)))
    (let ((compiled-arg (car (glx-compile-load-arg 10 glx-2))))
      (let ((*glx-stack* (list (list nil (list (cons (glx-32 #x03 #x32) glx-4))))))
        (should (equal (funcall (car compiled-arg) (cadr compiled-arg)) glx-4))))))

(ert-deftest compile-locals-arg-four-byte-offset ()
  "Compile locals arg (four byte offset)"
  :tags '(compile)
  (let ((*glx-memory* (vector #x31 #x0a #x34 #x22 #x32 #x03)))
    (let ((compiled-arg (car (glx-compile-load-arg 11 glx-2))))
      (let ((*glx-stack* (list (list nil (list (cons (glx-32 #x03 #x32 #x22 #x34) glx-4))))))
        (should (equal (funcall (car compiled-arg) (cadr compiled-arg)) glx-4))))))

(ert-deftest compile-a-ram-arg-one-byte-offset ()
  "Compile a RAM arg (one byte offset)"
  :tags '(compile)
  (let ((*glx-memory* (vector #x31 #x0d #x02 #xee #x32 #x03 #x45))
        (*glx-ram-start* glx-1))
    (let ((compiled-arg (car (glx-compile-load-arg 13 glx-2))))
      (should (equal (funcall (car compiled-arg) (cadr compiled-arg)) (glx-32 #x45 3 #x32 #xee))))))

(ert-deftest compile-a-ram-arg-two-byte-offset ()
  "Compile a RAM arg (two byte offset)"
  :tags '(compile)
  (let ((*glx-memory* (vector #x31 #x0e 0 #x03 #xee #x32 #x03 #x45))
        (*glx-ram-start* glx-1))
    (let ((compiled-arg (car (glx-compile-load-arg 14 glx-2))))
      (should (equal (funcall (car compiled-arg) (cadr compiled-arg)) (glx-32 #x45 3 #x32 #xee))))))

(ert-deftest compile-a-ram-arg-four-byte-offset ()
  "Compile a RAM arg (four byte offset)"
  :tags '(compile)
  (let ((*glx-memory* (vector #x31 #x0f 0 0 0 #x05 #xee #x32 #x03 #x45))
        (*glx-ram-start* glx-1))
    (let ((compiled-arg (car (glx-compile-load-arg 15 glx-2))))
      (should (equal (funcall (car compiled-arg) (cadr compiled-arg)) (glx-32 #x45 3 #x32 #xee))))))

(ert-deftest compile-instruction-no-args ()
  "Compile instruction (no args)"
  :tags '(compile)
  (let ((*glx-memory* (vector #x00)))
    (cl-multiple-value-bind (next-inst compiled-fun)
        (glx-compile-instruction glx-0)
      (should (equal next-inst glx-1))
      (glx-execute-compiled-instruction compiled-fun))))

(ert-deftest compile-instruction-no-args ()
  "Compile instruction (no args)"
  :tags '(compile)
  (let ((*glx-memory* (vector #x00)))
    (cl-multiple-value-bind (next-inst compiled-fun)
        (glx-compile-instruction glx-0)
      (should (equal next-inst glx-1))
      (glx-execute-compiled-instruction compiled-fun))))

(ert-deftest compile-instruction-no-args-with-effect ()
  "Compile instruction (no args) with effect"
  :tags '(compile)
  (let ((*glx-memory* (vector #x120)))
    (cl-multiple-value-bind (next-inst compiled-fun)
        (glx-compile-instruction glx-0)
      (should (equal next-inst glx-1))
      (should (equal (glx-execute-compiled-instruction compiled-fun) 'glx-return-to-emacs)))))

(ert-deftest compile-instruction-one-load-arg-constant ()
  "Compile instruction (one load arg [constant])"
  :tags '(compile)
  (let ((*glx-memory* (vector #x20 #x01 #x66)))
    (cl-multiple-value-bind (next-inst compiled-fun)
        (glx-compile-instruction glx-0)
      (should (equal next-inst glx-3))
      (let ((*glx-pc* (glx-32 100))
            (*glx-memory* (vector #x20 #x01 #x33))) ; constants are compiled before run time
        (glx-execute-compiled-instruction compiled-fun)
        (should (equal *glx-pc* (glx-32 200)))))))

(ert-deftest compile-instruction-one-load-arg-from-memory ()
  "Compile instruction (one load arg [from memory])"
  :tags '(compile)
  (let ((*glx-memory* (vector #x20 #x05 #x03 0 0 0 #x66)))
    (cl-multiple-value-bind (next-inst compiled-fun)
        (glx-compile-instruction glx-0)
      (should (equal next-inst glx-3))
      (let ((*glx-pc* (glx-32 100))
            (*glx-memory* (vector #x20 #x05 #x03 0 0 0 #x33)))
        (glx-execute-compiled-instruction compiled-fun)
        (should (equal *glx-pc* (glx-32 149)))))))

(ert-deftest compile-instruction-one-store-arg ()
  "Compile instruction (one store arg)"
  :tags '(compile)
  (let ((*glx-memory* (vector #x10 #x98 #x0d #x04 #x02))
        (*glx-ram-start* glx-5))
    (cl-multiple-value-bind (next-inst compiled-fun)
        (glx-compile-instruction glx-0)
      (should (equal next-inst glx-5))
      (let ((*glx-pc* (glx-32 100))
            (*glx-memory* (vector #x10 #x98 #x0d #x04 #x02 0 0 0 0 0 0))
            (*glx-stack* (list (list (list glx-8) (list (cons glx-4 glx-3))))))
        (glx-execute-compiled-instruction compiled-fun)
        (should (equal *glx-memory* [#x10 #x98 #x0d #x04 #x02 0 0 0 0 0 11]))))))
