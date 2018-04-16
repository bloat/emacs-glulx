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

(ert-deftest defopaque-should-expand ()
  "defopaque should expand"
  :tags '(glk opaque)
  (should-not (equal (macroexpand '(defopaque window)) '(defopaque window))))

(ert-deftest defopaque-should-create-a-var-to-store-instances ()
  "defopaque should create a var to store instances"
  :tags '(glk opaque)
  (should (member '(defvar glki-opq-window nil "The collection of glk window opaque objects") (macroexpand '(defopaque window)))))

(ert-deftest defopaque-should-create-a-method-to-iterate-instances-of-the-class ()
  "defopaque should create a method to iterate instances of the class"
  :tags '(glk opaque)
  (should (member '(defun glk-window-iterate (glk-window)
                     "Iterates through the instances of window. Returns the next window after the given one."
                     (glki-iterate-get-next glki-opq-window #'glki-opq-window-rock glk-window))
                  (macroexpand '(defopaque window)))))

(ert-deftest should-be-able-to-iterate-an-opaque-collection ()
  "Should be able to iterate an opaque collection"
  :tags '(glk opaque)
  (unwind-protect
      (progn
        (let ((glki-opq-window '((window1 . [a b]) (window2 . [c d]))))
          (should (equal (glki-iterate-get-next glki-opq-window (lambda (x) (aref x 1)) nil) '([a b] b)))
          (should (equal (glki-iterate-get-next glki-opq-window (lambda (x) (aref x 1)) (cdar glki-opq-window)) '([c d] d)))
          (should (equal (glki-iterate-get-next glki-opq-window (lambda (x) (aref x 1)) (cdar (cdr glki-opq-window))) nil))))))

(ert-deftest defopaque-should-create-a-constructor ()
  "defopaque should create a constructor"
  :tags '(glk opaque)
  (should (member '(defun glki-opq-an-object-create (glk-an-object-id rock a-slot)
                     "Creates and stores a new instance of an-object"
                     (when glk-an-object-id
                       (let ((new-object (glki-opq-an-object-private-create :glk-an-object-id glk-an-object-id :rock rock :a-slot a-slot)))
                         (push (cons glk-an-object-id new-object) glki-opq-an-object)
                         new-object)))
                  (macroexpand '(defopaque an-object a-slot)))))

(ert-deftest defopaque-should-create-a-destructor ()
  "defopaque should create a destructor"
  :tags '(glk opaque)
  (should (member '(defun glki-opq-an-object-dispose (glk-an-object)
                     "Disposes of an instance of an-object"
                     (setq glki-opq-an-object (rassq-delete-all glk-an-object glki-opq-an-object)))
                  (macroexpand '(defopaque an-object)))))

(ert-deftest defopaque-should-define-structure ()
  "defopaque should define structure"
  :tags '(glk opaque)
  (should (member '(cl-defstruct (glki-opq-an-object (:constructor glki-opq-an-object-private-create)
                                                     (:copier nil))
                     glk-an-object-id rock a-slot)
                  (macroexpand '(defopaque an-object a-slot)))))

(ert-deftest defopaque-should-define-lookup-function ()
  "defopaque should define lookup function"
  :tags '(glk opaque)
  (should (member '(defun glki-opq-an-object-lookup (glk-an-object-id)
                     "Lookup an instance of an-object from its id"
                     (cdr (assq glk-an-object-id glki-opq-an-object)))
                  (macroexpand '(defopaque an-object a-slot)))))
