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

(context "Opaque objects: "
         (tag opaque)

         (specify "defopaque should expand"
                  (expect (not (equal (macroexpand '(defopaque window)) '(defopaque window)))))

         (specify "defopaque should create a var to store instances"
                  (expect (member '(defvar glki-opq-window nil "The collection of glk window opaque objects") (macroexpand '(defopaque window)))))

         (specify "defopaque should create a method to clear plist values"
                  (expect (member '(defun glki-opq-clear-an-object-plist (glk-an-object)
                                     (put glk-an-object (quote rock) nil)
                                     (put glk-an-object (quote a-slot) nil)
                                     (put glk-an-object (quote another-slot) nil))
                                  (macroexpand '(defopaque an-object a-slot another-slot)))))

         (specify "defopaque should create a method to set plist values"
                  (expect (member '(defun glki-opq-an-object-set-a-slot (glk-an-object value)
                                     "Sets the a-slot value of this an-object"
                                     (put glk-an-object 'a-slot value))
                                  (macroexpand '(defopaque an-object a-slot)))))

         (specify "defopaque should create a method to get plist values"
                  (expect (member '(defun glki-opq-an-object-get-a-slot (glk-an-object)
                                     "Gets the a-slot value of this an-object"
                                     (get glk-an-object 'a-slot))
                                  (macroexpand '(defopaque an-object a-slot)))))

         (specify "defopaque should create a method to set the rock"
                  (expect (member '(defun glki-opq-an-object-set-rock (glk-an-object value)
                                     "Sets the rock value of this an-object"
                                     (put glk-an-object 'rock value))
                                  (macroexpand '(defopaque an-object)))))

         (specify "defopaque should create a method to get the rock"
                  (expect (member '(defun glki-opq-an-object-get-rock (glk-an-object)
                                     "Gets the rock value of this an-object"
                                     (get glk-an-object 'rock))
                                  (macroexpand '(defopaque an-object)))))

         (specify "defopaque should create a method to iterate instances of the class"
                  (expect (member '(defun glk-window-iterate (glk-window)
                                     "Iterates through the instances of window. Returns the next window after the given one."
                                     (glki-iterate-get-next glki-opq-window glk-window))
                                  (macroexpand '(defopaque window)))))

         (specify "Should be able to iterate an opaque collection"
                  (unwind-protect
                      (progn
                        (put 'window1 'rock 'rock1)
                        (put 'window2 'rock 'rock2)
                        (let ((glki-opq-window '(window1 window2)))
                          (expect (glki-iterate-get-next glki-opq-window nil) equals '(window1 rock1))
                          (expect (glki-iterate-get-next glki-opq-window 'window1) equals '(window2 rock2))
                          (expect (glki-iterate-get-next glki-opq-window 'window2) equals nil)))
                    (put 'window1 'rock nil)
                    (put 'window2 'rock nil)))

         (specify "defopaque should create a constructor"
                  (expect (member '(defun glki-opq-an-object-create (glk-an-object-id)
                                     "Creates and stores a new instance of an-object"
                                     (when glk-an-object-id
                                       (glki-opq-clear-an-object-plist glk-an-object-id)
                                       (push glk-an-object-id glki-opq-an-object))
                                     glk-an-object-id)
                                  (macroexpand '(defopaque an-object)))))

         (specify "defopaque should create a destructor"
                  (expect (member '(defun glki-opq-an-object-dispose (glk-an-object-id)
                                     "Disposes of an instance of an-object"
                                     (glki-opq-clear-an-object-plist glk-an-object-id)
                                     (setq glki-opq-an-object (remove glk-an-object-id glki-opq-an-object)))
                                  (macroexpand '(defopaque an-object))))))
