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

(ert-deftest defopaque-should-create-a-method-to-clear-plist-values ()
  "defopaque should create a method to clear plist values"
  :tags '(glk opaque)
  (should (member '(defun glki-opq-clear-an-object-plist (glk-an-object)
                     (put glk-an-object (quote rock) nil)
                     (put glk-an-object (quote a-slot) nil)
                     (put glk-an-object (quote another-slot) nil))
                  (macroexpand '(defopaque an-object a-slot another-slot)))))

(ert-deftest defopaque-should-create-a-method-to-set-plist-values ()
  "defopaque should create a method to set plist values"
  :tags '(glk opaque)
  (should (member '(defun glki-opq-an-object-set-a-slot (glk-an-object value)
                     "Sets the a-slot value of this an-object"
                     (put glk-an-object 'a-slot value))
                  (macroexpand '(defopaque an-object a-slot)))))

(ert-deftest defopaque-should-create-a-method-to-get-plist-values ()
  "defopaque should create a method to get plist values"
  :tags '(glk opaque)
  (should (member '(defun glki-opq-an-object-get-a-slot (glk-an-object)
                     "Gets the a-slot value of this an-object"
                     (get glk-an-object 'a-slot))
                  (macroexpand '(defopaque an-object a-slot)))))

(ert-deftest defopaque-should-create-a-method-to-set-the-rock ()
  "defopaque should create a method to set the rock"
  :tags '(glk opaque)
  (should (member '(defun glki-opq-an-object-set-rock (glk-an-object value)
                     "Sets the rock value of this an-object"
                     (put glk-an-object 'rock value))
                  (macroexpand '(defopaque an-object)))))

(ert-deftest defopaque-should-create-a-method-to-get-the-rock ()
  "defopaque should create a method to get the rock"
  :tags '(glk opaque)
  (should (member '(defun glki-opq-an-object-get-rock (glk-an-object)
                     "Gets the rock value of this an-object"
                     (get glk-an-object 'rock))
                  (macroexpand '(defopaque an-object)))))

(ert-deftest defopaque-should-create-a-method-to-iterate-instances-of-the-class ()
  "defopaque should create a method to iterate instances of the class"
  :tags '(glk opaque)
  (should (member '(defun glk-window-iterate (glk-window)
                     "Iterates through the instances of window. Returns the next window after the given one."
                     (glki-iterate-get-next glki-opq-window glk-window))
                  (macroexpand '(defopaque window)))))

(ert-deftest should-be-able-to-iterate-an-opaque-collection ()
  "Should be able to iterate an opaque collection"
  :tags '(glk opaque)
  (unwind-protect
      (progn
        (put 'window1 'rock 'rock1)
        (put 'window2 'rock 'rock2)
        (let ((glki-opq-window '(window1 window2)))
          (should (equal (glki-iterate-get-next glki-opq-window nil) '(window1 rock1)))
          (should (equal (glki-iterate-get-next glki-opq-window 'window1) '(window2 rock2)))
          (should (equal (glki-iterate-get-next glki-opq-window 'window2) nil))))
    (put 'window1 'rock nil)
    (put 'window2 'rock nil)))

(ert-deftest defopaque-should-create-a-constructor ()
  "defopaque should create a constructor"
  :tags '(glk opaque)
  (should (member '(defun glki-opq-an-object-create (glk-an-object-id)
                     "Creates and stores a new instance of an-object"
                     (when glk-an-object-id
                       (glki-opq-clear-an-object-plist glk-an-object-id)
                       (push glk-an-object-id glki-opq-an-object))
                     glk-an-object-id)
                  (macroexpand '(defopaque an-object)))))

(ert-deftest defopaque-should-create-a-destructor ()
  "defopaque should create a destructor"
  :tags '(glk opaque)
  (should (member '(defun glki-opq-an-object-dispose (glk-an-object-id)
                     "Disposes of an instance of an-object"
                     (glki-opq-clear-an-object-plist glk-an-object-id)
                     (setq glki-opq-an-object (remove glk-an-object-id glki-opq-an-object)))
                  (macroexpand '(defopaque an-object)))))
