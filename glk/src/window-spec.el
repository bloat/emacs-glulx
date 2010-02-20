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

(context "Opening windows: "
         (tag windows)
         (labels ((create-two-windows-proportional (new-position size-percentage)
                                                   (glk-window-open (glk-window-open nil nil 0 'glk-wintype-text-buffer 0 'window1 nil 'stream1 'stream2)
                                                                    (list new-position 'glk-winmethod-proportional)
                                                                    size-percentage 'glk-wintype-text-buffer 0 'window2 'pair 'stream3 'stream4))
                  (create-two-windows-fixed (new-position size &optional type)
                                            (when (null type) (setq type 'glk-wintype-text-buffer))
                                            (glk-window-open (glk-window-open nil nil 0 'glk-wintype-text-buffer 0 'window1 nil nil nil)
                                                             (list new-position 'glk-winmethod-fixed) size type 0 'window2 'pair nil nil))
                  (first-window () (glki-get-emacs-window 'window1))
                  (second-window () (glki-get-emacs-window 'window2))
                  (bottom-window-p (window) (coordinates-in-window-p (cons 0 (- (frame-height glk-frame) 2)) window))
                  (top-window-p (window) (coordinates-in-window-p (cons 0 1) window))
                  (right-window-p (window) (coordinates-in-window-p (cons (- (frame-width glk-frame) 1) 1) window))
                  (left-window-p (window) (coordinates-in-window-p (cons 0 1) window))
                  (number-of-windows-on-glk-frame () (length (window-list glk-frame 'no-minibuffer (first-window))))
                  (get-point-in-window (windowid) (save-current-buffer (set-buffer (glki-opq-window-get-buffer windowid)) (point))))

           (macrolet ((with-glk-start-and-end (&body body)
                                              `(unwind-protect
                                                   (progn (glki-init (make-frame (list (cons 'height 61) (cons 'width 60))))
                                                          ,@body)
                                                 (glki-end)
                                                 (expect glki-opq-window equals nil "Windows not cleaned up")
                                                 (expect glki-opq-stream equals nil "Streams not cleaned up")
                                                 (expect (remove-if #'(lambda (b) (not (string-match "\\*glk\\*" (buffer-name b)))) (buffer-list)) equals nil "Buffers not cleaned up")))

                      (with-two-windows (second-window-position &body body)
                                        `(with-glk-start-and-end
                                          (create-two-windows-proportional ,second-window-position 50)
                                          ,@body)))

             (specify "Should be able to open the first glk window"
                      (with-glk-start-and-end
                       (expect (glk-window-open nil 0 0 'glk-wintype-text-buffer 0 'window1 nil nil nil) equals 'window1)))

             (specify "Should be able to clean up one window"
                      (with-glk-start-and-end
                       (glk-window-open nil 0 0 'glk-wintype-text-buffer 0 'window1 nil nil nil))
                      (expect (not (glki-opq-window-get-buffer 'window1)))
                      (let ((buf))
                        (unwind-protect
                            (progn
                              (setq buf (generate-new-buffer "*glk*"))
                              (expect (buffer-name buf) equals "*glk*"))
                          (kill-buffer buf))))

             (specify "First glk window should occupy entire frame"
                      (with-glk-start-and-end
                       (glk-window-open nil 0 0 'glk-wintype-text-buffer 0 'window1 nil nil nil)
                       (expect (number-of-windows-on-glk-frame) equal 1)))

             (specify "First window should have null children"
                      (with-glk-start-and-end
                       (glk-window-open nil 0 0 'glk-wintype-text-buffer 0 'window1 nil nil nil)
                       (expect (not (glki-opq-window-get-first-child 'window1)))
                       (expect (not (glki-opq-window-get-second-child 'window1)))))

             (specify "Should not be able to open the second \"first\" glk window"
                      (with-glk-start-and-end
                       (glk-window-open nil 0 0 'glk-wintype-text-buffer 0 'window1 nil nil nil)
                       (expect (not (glk-window-open nil 0 0 'glk-wintype-text-buffer 0 'window2 nil nil nil)))))

             (specify "Should be able to split a glk window"
                      (with-glk-start-and-end
                       (expect (create-two-windows-proportional 'glk-winmethod-above 50))))

             (specify "Should be able to clean up after a split"
                      (with-glk-start-and-end
                       (create-two-windows-proportional 'glk-winmethod-above 50))
                      (expect (not (glki-opq-window-get-buffer 'window1)))
                      (expect (not (glki-opq-window-get-buffer 'window2)))
                      (expect (not (glki-opq-window-get-buffer 'pair))))

             (specify "Should be able to split a split"
                      (with-glk-start-and-end
                       (create-two-windows-proportional 'glk-winmethod-above 50)
                       (glk-window-open 'window2 '(glk-winmethod-above glk-winmethod-proportional) 50 'glk-wintype-text-buffer 0 'window3 'pair2 nil nil)
                       (expect (glki-opq-window-get-first-child 'pair) equals 'window1 "key child of pair")
                       (expect (glki-opq-window-get-second-child 'pair) equals 'pair2 "other child of pair")
                       (expect (glki-opq-window-get-first-child 'pair2) equals 'window2 "key child of pair2")
                       (expect (glki-opq-window-get-second-child 'pair2) equals 'window3 "other child of pair2")
                       (expect glk-root-window equals 'pair)))

             (specify "Splitting a window should create a new window"
                      (with-two-windows 'glk-winmethod-above
                                        (expect (number-of-windows-on-glk-frame) equal 2)))

             (specify "A parent window should have the right children"
                      (with-two-windows 'glk-winmethod-right
                                        (expect (glki-opq-window-get-first-child 'pair) equals 'window1)
                                        (expect (glki-opq-window-get-second-child 'pair) equals 'window2)))

             (specify "Creating a new window above should leave the old window below"
                      (with-two-windows 'glk-winmethod-above (expect (bottom-window-p (first-window)))))

             (specify "Creating a new window above should create the new window above"
                      (with-two-windows 'glk-winmethod-above (expect (top-window-p (second-window)))))

             (specify "Creating a new window below should leave the old window above"
                      (with-two-windows 'glk-winmethod-below (expect (top-window-p (first-window)))))

             (specify "Creating a new window below should create the new window below"
                      (with-two-windows 'glk-winmethod-below (expect (bottom-window-p (second-window)))))

             (specify "Creating a new window on the left should leave the old window on the right"
                      (with-two-windows 'glk-winmethod-left (expect (right-window-p (first-window)))))

             (specify "Creating a new window on the left should create the new window on the left"
                      (with-two-windows 'glk-winmethod-left (expect (left-window-p (second-window)))))

             (specify "Creating a new window on the right should leave the old window on the left"
                      (with-two-windows 'glk-winmethod-right (expect (left-window-p (first-window)))))

             (specify "Creating a new window on the right should create the new window on the right"
                      (with-two-windows 'glk-winmethod-right (expect (right-window-p (second-window)))))

;;              (specify "Creating a new window above with a size should create a new window with the right size"
;;                       (with-glk-start-and-end
;;                        (create-two-windows-proportional 'glk-winmethod-above 20)
;;                        (expect (window-height (first-window)) equal 48)
;;                        (expect (window-height (second-window)) equal 12)))

;;              (specify "Creating a new window below with a size should create a new window with the right size"
;;                       (with-glk-start-and-end
;;                        (create-two-windows-proportional 'glk-winmethod-below 20)
;;                        (expect (window-height (first-window)) equal 48)
;;                        (expect (window-height (second-window)) equal 12)))

;;              (specify "Proportional window creation horizontally should create a new window with the right size"
;;                       (with-glk-start-and-end
;;                        (create-two-windows-proportional 'glk-winmethod-left 20)
;;                        (expect (window-width (first-window)) equal 48) ;; fringes appear to take 2 chars
;;                        (expect (window-width (second-window)) equal 9)))

;;              (specify "Proportional window creation horizontally should create a new window with the right size"
;;                       (with-glk-start-and-end
;;                        (create-two-windows-proportional 'glk-winmethod-right 20)
;;                        (expect (window-width (first-window)) equal 45) ;; fringes appear to take 2 chars
;;                        (expect (window-width (second-window)) equal 12)))

;;              (specify "Fixed window creation horizontally should create a new window with the right size"
;;                       (with-glk-start-and-end
;;                        (create-two-windows-fixed 'glk-winmethod-above 10)
;;                        (expect (window-height (first-window)) equal 50)
;;                        (expect (window-height (second-window)) equal 10)))

;;              (specify "Fixed window creation vertically should create a new window with the right size"
;;                       (with-glk-start-and-end
;;                        (create-two-windows-fixed 'glk-winmethod-right 20)
;;                        (expect (window-width (first-window)) equal 17)
;;                        (expect (window-width (second-window)) equal 40)))

             (specify "glki-get-window-id should return correct windowid"
                      (with-two-windows
                       'glk-winmethod-right
                       (expect (glki-get-window-id (glki-opq-window-get-buffer 'window1)) equals 'window1)
                       (expect (glki-get-window-id (glki-opq-window-get-buffer 'window2)) equals 'window2)))

             (specify "Should be able to close a window"
                      (with-glk-start-and-end
                       (create-two-windows-proportional 'glk-winmethod-right 50)
                       (glk-window-close 'window2)
                       (expect (not (glki-opq-window-get-buffer 'window2)))
                       (expect (glki-opq-window-get-buffer 'window1))
                       (expect (number-of-windows-on-glk-frame) equals 1)))

             (specify "Should be able to clear a window"
                      (with-two-windows
                       'glk-winmethod-above
                       (glk-set-window 'window1)
                       (glk-put-string "Hello")
                       (glk-window-clear 'window1)
                       (save-current-buffer
                         (set-buffer "*glk*")
                         (expect (buffer-string) equals ""))))

;;              (specify "Should be able to get window size"
;;                       (with-glk-start-and-end
;;                        (create-two-windows-proportional 'glk-winmethod-above 50)
;;                        (expect (glk-window-get-size 'window1) equals '(60 29))
;;                        (expect (glk-window-get-size 'window2) equals '(60 29))))

             (specify "Should not be able to change cursor position in text buffer window"
                      (unwind-protect
                          (let* ((test-window (glki-generate-new-window 'glk-wintype-text-buffer 'window1 'stream))
                                 (current-point (get-point-in-window 'window1)))
                            (glk-window-move-cursor 'window1 5 5)
                            (expect (get-point-in-window 'window1) equals current-point))
                        (glki-dispose-window 'window1)))

             (specify "Text Grid window should be the correct size"
                      (with-glk-start-and-end
                       (create-two-windows-fixed 'glk-winmethod-above 5 'glk-wintype-text-grid)
                       (expect (caddr (glk-window-get-size 'window2)) equals 5)))

             (specify "Should be able to change cursor position in a text grid window"
                      (with-glk-start-and-end
                       (create-two-windows-fixed 'glk-winmethod-above 5 'glk-wintype-text-grid)
                       (glk-window-move-cursor 'window2 0 0)
                       (expect (get-point-in-window 'window2) equals 1)
                       (glk-window-move-cursor 'window2 1 0)
                       (expect (get-point-in-window 'window2) equals 2)
                       (glk-window-move-cursor 'window2 0 1)
                       (expect (get-point-in-window 'window2) equals 3)
                       (glk-window-move-cursor 'window2 2 0)
                       (expect (get-point-in-window 'window2) equals 3)
                       (glk-window-move-cursor 'window2 1 1)
                       (expect (get-point-in-window 'window2) equals 5))))))
