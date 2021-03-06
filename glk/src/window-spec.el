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

(cl-macrolet ((with-test-functions (&body body)
                                   `(cl-labels ((create-two-windows-proportional (new-position size-percentage)
                                                                                 (let ((window1 (glk-window-open nil nil 0 'glk-wintype-text-buffer 0 'window1 nil 'stream1 'stream2)))
                                                                                   (list window1 (glk-window-open window1
                                                                                                                  (list new-position 'glk-winmethod-proportional)
                                                                                                                  size-percentage 'glk-wintype-text-buffer 0 'window2 'pair 'stream3 'stream4))))
                                                (create-two-windows-fixed (new-position size &optional type)
                                                                          (let ((window1 (glk-window-open nil nil 0 'glk-wintype-text-buffer 0 'window1 nil nil nil)))
                                                                            (list window1 (glk-window-open window1
                                                                                                           (list new-position 'glk-winmethod-fixed) size (or type glk-wintype-text-buffer) 0 'window2 'pair nil nil))))
                                                (first-window () (glki-get-emacs-window (cdr (assq 'window1 glki-opq-window))))
                                                (second-window () (glki-get-emacs-window (cdr (assq 'window2 glki-opq-window))))
                                                (bottom-window-p (window) (coordinates-in-window-p (cons 0 (- (frame-height glk-frame) 2)) window))
                                                (top-window-p (window) (coordinates-in-window-p (cons 0 1) window))
                                                (right-window-p (window) (coordinates-in-window-p (cons (- (frame-width glk-frame) 1) 1) window))
                                                (left-window-p (window) (coordinates-in-window-p (cons 0 1) window))
                                                (number-of-windows-on-glk-frame () (length (window-list glk-frame 'no-minibuffer (first-window))))
                                                (get-point-in-window (windowid) (with-current-buffer (glki-opq-window-buffer windowid) (list (line-number-at-pos) (current-column)))))
                                      ,@body))
              
              
              (with-glk-start-and-end (&body body)
                                      
                                      `(with-test-functions
                                        
                                        (unwind-protect
                                            (progn (glki-init (make-frame (list (cons 'height 61) (cons 'width 60))))
                                                   ,@body)
                                          (glki-end)
                                          (should-not glki-opq-window)
                                          (should-not glki-opq-stream)
                                          (should-not (cl-remove-if #'(lambda (b) (not (string-match "\\*glk\\*" (buffer-name b)))) (buffer-list))))))
              
              (with-two-windows (second-window-position &body body)
                                `(with-glk-start-and-end
                                  (cl-multiple-value-bind (window1 window2) (create-two-windows-proportional ,second-window-position 50)
                                    (let ((pair (glki-opq-window-lookup 'pair)))
                                      ,@body)))))
  
  (ert-deftest should-be-able-to-open-the-first-glk-window ()
    "Should be able to open the first glk window"
    :tags '(glk window)
    (with-glk-start-and-end
     (should (equal (glki-opq-window-glk-window-id (glk-window-open nil 0 0 'glk-wintype-text-buffer 0 'window1 nil nil nil)) 'window1))))

  (ert-deftest should-be-able-to-clean-up-one-window ()
    "Should be able to clean up one window"
    :tags '(glk window)
    (with-glk-start-and-end
     (glk-window-open nil 0 0 'glk-wintype-text-buffer 0 'window1 nil nil nil))
    (should-not glki-opq-window)
    (let ((buf))
      (unwind-protect
          (progn
            (setq buf (generate-new-buffer "*glk*"))
            (should (equal (buffer-name buf) "*glk*")))
        (kill-buffer buf))))

  (ert-deftest first-glk-window-should-occupy-entire-frame ()
    "First glk window should occupy entire frame"
    :tags '(glk window)
    (with-glk-start-and-end
     (glk-window-open nil 0 0 'glk-wintype-text-buffer 0 'window1 nil nil nil)
     (should (= (number-of-windows-on-glk-frame) 1))))

  (ert-deftest first-window-should-have-null-children ()
    "First window should have null children"
    :tags '(glk window)
    (with-glk-start-and-end
     (let ((window1 (glk-window-open nil 0 0 'glk-wintype-text-buffer 0 'window1 nil nil nil)))
       (should-not (glki-opq-window-first-child window1))
       (should-not (glki-opq-window-second-child window1)))))

  (ert-deftest should-not-be-able-to-open-a-second-first-glk-window ()
    "Should not be able to open a second 'first' glk window"
    :tags '(glk window)
    (with-glk-start-and-end
     (glk-window-open nil 0 0 'glk-wintype-text-buffer 0 'window1 nil nil nil)
     (should-not (glk-window-open nil 0 0 'glk-wintype-text-buffer 0 'window2 nil nil nil))))

  (ert-deftest should-be-able-to-split-a-glk-window ()
    "Should be able to split a glk window"
    :tags '(glk window)
    (with-glk-start-and-end
     (should (create-two-windows-proportional 'glk-winmethod-above 50))))

  (ert-deftest should-be-able-to-clean-up-after-a-split ()
    "Should be able to clean up after a split"
    :tags '(glk window)
    (with-glk-start-and-end
     (create-two-windows-proportional 'glk-winmethod-above 50))
    (should-not glki-opq-window)
    (let ((buf))
      (unwind-protect
          (progn
            (setq buf (generate-new-buffer "*glk*"))
            (should (equal (buffer-name buf) "*glk*")))
        (kill-buffer buf))))
  
  (ert-deftest should-be-able-to-split-a-split ()
    "Should be able to split a split"
    :tags '(glk window)
    (with-glk-start-and-end
     (cl-multiple-value-bind (window1 window2)
         (create-two-windows-proportional 'glk-winmethod-above 50)
       (let ((pair (glki-opq-window-lookup 'pair))
             (window3 (glk-window-open window2 '(glk-winmethod-above glk-winmethod-proportional) 50 'glk-wintype-text-buffer 0 'window3 'pair2 nil nil))
             (pair2 (glki-opq-window-lookup 'pair2)))
         (should (eq (glki-opq-window-first-child pair) window1))
         (should (eq (glki-opq-window-second-child pair) pair2))
         (should (eq (glki-opq-window-first-child pair2) window2))
         (should (eq (glki-opq-window-second-child pair2) window3))
         (should (eq glk-root-window pair))))))

  (ert-deftest splitting-a-window-should-create-a-new-window ()
    "Splitting a window should create a new window"
    :tags '(glk window)
    (with-two-windows 'glk-winmethod-above
                      (should (= (number-of-windows-on-glk-frame) 2))))

  (ert-deftest a-parent-window-should-have-the-right-children ()
    "A parent window should have the right children"
    :tags '(glk window)
    (with-two-windows 'glk-winmethod-right
                      (should (equal (glki-opq-window-first-child pair) window1))
                      (should (equal (glki-opq-window-second-child pair) window2))))

  (ert-deftest a-child-window-should-have-the-right-sibling ()
    "A child window should have the right sibling"
    :tags '(glk window)
    (with-two-windows 'glk-winmethod-right
                      (should (eq (glk-window-get-sibling window1) window2))
                      (should (eq (glk-window-get-sibling window2) window1))))

  (ert-deftest a-child-window-should-have-the-right-parent ()
    "A child window should have the right parent"
    :tags '(glk window)
    (with-two-windows 'glk-winmethod-right
                      (should (eq (glk-window-get-parent window1) pair))
                      (should (eq (glk-window-get-parent window2) pair))))

  (ert-deftest creating-a-new-window-above-should-leave-the-old-window-below ()
    "Creating a new window above should leave the old window below"
    :tags '(glk window)
    (with-two-windows 'glk-winmethod-above (should (bottom-window-p (first-window)))))

  (ert-deftest creating-a-new-window-above-should-create-the-new-window-above ()
    "Creating a new window above should create the new window above"
    :tags '(glk window)
    (with-two-windows 'glk-winmethod-above (should (top-window-p (second-window)))))

  (ert-deftest creating-a-new-window-below-should-leave-the-old-window-above ()
    "Creating a new window below should leave the old window above"
    :tags '(glk window)
    (with-two-windows 'glk-winmethod-below (should (top-window-p (first-window)))))

  (ert-deftest creating-a-new-window-below-should-create-the-new-window-below ()
    "Creating a new window below should create the new window below"
    :tags '(glk window)
    (with-two-windows 'glk-winmethod-below (should (bottom-window-p (second-window)))))

  (ert-deftest creating-a-new-window-on-the-left-should-leave-the-old-window-on-the-right ()
    "Creating a new window on the left should leave the old window on the right"
    :tags '(glk window)
    (with-two-windows 'glk-winmethod-left (should (right-window-p (first-window)))))

  (ert-deftest creating-a-new-window-on-the-left-should-create-the-new-window-on-the-left ()
    "Creating a new window on the left should create the new window on the left"
    :tags '(glk window)
    (with-two-windows 'glk-winmethod-left (should (left-window-p (second-window)))))

  (ert-deftest creating-a-new-window-on-the-right-should-leave-the-old-window-on-the-left ()
    "Creating a new window on the right should leave the old window on the left"
    :tags '(glk window)
    (with-two-windows 'glk-winmethod-right (should (left-window-p (first-window)))))

  (ert-deftest creating-a-new-window-on-the-right-should-create-the-new-window-on-the-right ()
    "Creating a new window on the right should create the new window on the right"
    :tags '(glk window)
    (with-two-windows 'glk-winmethod-right (should (right-window-p (second-window)))))

  ;;              (specify "Creating a new window above with a size should create a new window with the right size"
  ;;                       (with-glk-start-and-end
  ;;                        (create-two-windows-proportional 'glk-winmethod-above 20)
  ;;                        (should (window-height (first-window)) equal 48)
  ;;                        (should (window-height (second-window)) equal 12)))

  ;;              (specify "Creating a new window below with a size should create a new window with the right size"
  ;;                       (with-glk-start-and-end
  ;;                        (create-two-windows-proportional 'glk-winmethod-below 20)
  ;;                        (should (window-height (first-window)) equal 48)
  ;;                        (should (window-height (second-window)) equal 12)))

  ;;              (specify "Proportional window creation horizontally should create a new window with the right size"
  ;;                       (with-glk-start-and-end
  ;;                        (create-two-windows-proportional 'glk-winmethod-left 20)
  ;;                        (should (window-width (first-window)) equal 48) ;; fringes appear to take 2 chars
  ;;                        (should (window-width (second-window)) equal 9)))

  ;;              (specify "Proportional window creation horizontally should create a new window with the right size"
  ;;                       (with-glk-start-and-end
  ;;                        (create-two-windows-proportional 'glk-winmethod-right 20)
  ;;                        (should (window-width (first-window)) equal 45) ;; fringes appear to take 2 chars
  ;;                        (should (window-width (second-window)) equal 12)))

  ;;              (specify "Fixed window creation horizontally should create a new window with the right size"
  ;;                       (with-glk-start-and-end
  ;;                        (create-two-windows-fixed 'glk-winmethod-above 10)
  ;;                        (should (window-height (first-window)) equal 50)
  ;;                        (should (window-height (second-window)) equal 10)))

  ;;              (specify "Fixed window creation vertically should create a new window with the right size"
  ;;                       (with-glk-start-and-end
  ;;                        (create-two-windows-fixed 'glk-winmethod-right 20)
  ;;                        (should (window-width (first-window)) equal 17)
  ;;                        (should (window-width (second-window)) equal 40)))

  (ert-deftest glki-get-window-should-return-correct-window ()
    "glki-get-window should return correct windowid"
    :tags '(glk window)
    (with-two-windows
     'glk-winmethod-right
     (should (eq (glki-get-window (glki-opq-window-buffer window1)) window1))
     (should (eq (glki-get-window (glki-opq-window-buffer window2)) window2))))

  (ert-deftest should-be-able-to-close-a-window ()
    "Should be able to close a window"
    :tags '(glk window)
    (with-glk-start-and-end
     (cl-multiple-value-bind (window1 window2)
         (create-two-windows-proportional 'glk-winmethod-right 50)
       (glk-window-close window2)
       (should-not (buffer-live-p (glki-opq-window-buffer window2)))
       (should (glki-opq-window-buffer window1))
       (should (= (number-of-windows-on-glk-frame) 1)))))

  (ert-deftest should-be-able-to-clear-a-window ()
    "Should be able to clear a window"
    :tags '(glk window)
    (with-two-windows
     'glk-winmethod-above
     (glk-set-window window1)
     (glk-put-string "Hello")
     (glk-window-clear window1)
     (with-current-buffer "*glk*"
       (should (equal (buffer-string) "")))))

  (ert-deftest should-not-be-able-to-change-cursor-position-in-text-buffer-window ()
    "Should not be able to change cursor position in text buffer window"
    :tags '(glk window)
    (with-test-functions
     (unwind-protect
         (let* ((window1 (glki-generate-new-window 'glk-wintype-text-buffer 'window1 'stream 0))
                (current-point (get-point-in-window window1)))
           (glk-window-move-cursor window1 5 5)
           (should (equal (get-point-in-window window1) current-point)))
       (glki-kill-all-windows))))

  (ert-deftest text-grid-window-should-be-the-correct-size ()
    "Text Grid window should be the correct size"
    :tags '(glk window)
    (with-glk-start-and-end
     (cl-multiple-value-bind (window1 window2)
         (create-two-windows-fixed 'glk-winmethod-above 5 'glk-wintype-text-grid)
       (should (= (cl-caddr (glk-window-get-size window2)) 5)))))

  (ert-deftest should-be-able-to-change-cursor-position-in-a-text-grid-window ()
    "Should be able to change cursor position in a text grid window"
    :tags '(glk window)
    (with-glk-start-and-end
     (cl-multiple-value-bind (window1 window2)
         (create-two-windows-fixed 'glk-winmethod-above 5 'glk-wintype-text-grid)
       (glk-window-move-cursor window2 0 0)
       (should (equal (get-point-in-window window2) (list 1 0)))
       (glk-window-move-cursor window2 1 0)
       (should (equal (get-point-in-window window2) (list 1 1)))
       (glk-window-move-cursor window2 0 1)
       (should (equal (get-point-in-window window2) (list 2 0)))
       (glk-window-move-cursor window2 2 0)
       (should (equal (get-point-in-window window2) (list 1 2)))
       (glk-window-move-cursor window2 1 1)
       (should (equal (get-point-in-window window2) (list 2 1)))))))
