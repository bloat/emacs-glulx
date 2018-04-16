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

(require 'glk-opaque)
(require 'glk-stream)
(require 'simple)

(defvar glk-frame nil "The frame for GLK windows")

(defun glki-init (&optional frame)
  (setq glk-frame (if (null frame) (selected-frame) frame)))

(defvar glk-mode-map nil "Map for GLK mode")

(define-derived-mode glk-mode nil "GLK"
  "Major mode for interacting with GLK based programs.
\\{glk-mode-map}"
  (use-hard-newlines)
  (visual-line-mode))

(defvar glk-text-grid-mode-map nil "Map for GLK text grid mode")

(define-derived-mode glk-text-grid-mode nil "GLK-TG"
  "Major mode for interacting with GLK based programs.
\\{glk-text-grid-mode-map}"
  (use-hard-newlines)
  (overwrite-mode))

;; All GLK Windows

(defopaque window first-child second-child buffer type is-key glk-line-event glk-char-event stream)

(defvar glk-root-window nil "Stores the GLK root window")

;; Creating, getting hold of, and deleting windows

(defun glki-find-window (predicate)
  (cl-some (lambda (win) (funcall predicate (cdr win))) glki-opq-window))

(defun glki-get-window (buffer)
  "Returns the glk window for the given emacs buffer"
  (glki-find-window #'(lambda (window) (when (eq buffer (glki-opq-window-buffer window)) window))))

(defun glki-kill-all-windows ()
  "Cleans up all glk windows and destroys the associated buffers"
  (mapcar (lambda (c) (glki-dispose-window (cdr c))) glki-opq-window))

(defun glki-generate-new-window (type new-window-id new-stream-id rock)
  "Generate a new glk window"
  (let* ((buffer (if (eq type 'glk-wintype-pair)
                    nil
                   (generate-new-buffer "*glk*")))
         (window (glki-opq-window-create new-window-id rock nil nil buffer type nil nil nil (glki-create-window-stream type buffer new-stream-id))))
    (glki-set-glk-mode window)
    window))

(defun glki-dispose-window (window)
  (when (glki-opq-window-stream window)
    (glki-opq-stream-dispose (glki-opq-window-stream window)))
  (when (glki-opq-window-buffer window)
    (kill-buffer (glki-opq-window-buffer window)))
  (glki-opq-window-dispose window))

(defun glki-create-first-window (type new-window-id new-stream-id rock)
  "Generate the first GLK window"
  (let ((window-on-glk-frame (frame-first-window glk-frame))
        (new-window (glki-generate-new-window type new-window-id new-stream-id rock)))
     (delete-other-windows window-on-glk-frame)
    (set-window-buffer window-on-glk-frame (glki-opq-window-buffer new-window))
    (setq glk-root-window new-window)))

;; Getting window properties

(defun glki-set-glk-mode (glk-window)
  "Sets glk-mode on the buffer for the given glk-window"
  (when (glki-opq-window-buffer glk-window)
    (with-current-buffer (glki-opq-window-buffer glk-window)
      (if (eq (glki-opq-window-type glk-window) 'glk-wintype-text-grid)
          (glk-text-grid-mode)
        (glk-mode)))))

(defun glki-get-emacs-window (glk-window)
  "Returns the emacs windows displaying the given glk window"
  (get-buffer-window (glki-opq-window-buffer glk-window) glk-frame))

(defun glki-fix-parent (old-child new-child)
  (glki-find-window (lambda (win) (cond ((eq (glki-opq-window-first-child win) old-child)
                                         (setf (glki-opq-window-first-child win) new-child))
                                        ((eq (glki-opq-window-second-child win) old-child)
                                         (setf (glki-opq-window-second-child win) new-child))))))

;; GLK windows

(defun glki-vertical-split (method)
  "Is the requested split a vertical split?"
  (or (memq 'glk-winmethod-left method)
      (memq 'glk-winmethod-right method)))

(defun glki-calc-new-window-size (method old-window size type)
  "Calculate the size parameter for the emacs window for this glk window"
  (if (memq 'glk-winmethod-fixed method)
      (+ (if (eq 'glk-wintype-text-grid type)
             1 0)
         size)
    (/ (* (if (or (memq 'glk-winmethod-below method) (memq 'glk-winmethod-right method))
              (- 100 size)
            size)
          (if (or (memq 'glk-winmethod-above method) (memq 'glk-winmethod-below method))
              (window-height old-window)
            (window-width old-window)))
       100)))

(defun glk-window-open (split method size wintype rock new-window-id new-parent-id new-window-stream-id new-parent-stream-id)
  "Creates a new glk window and a new buffer for it to display"
  (cond ((null split)
         (when (null glk-root-window)
           (glki-create-first-window wintype new-window-id new-window-stream-id rock)))
        (t
         (let* ((new-glk-window (glki-generate-new-window wintype new-window-id new-window-stream-id rock))
                (new-pair-window (glki-generate-new-window 'glk-wintype-pair new-parent-id new-parent-stream-id 0))
                (old-emacs-window (glki-get-emacs-window split))
                (new-emacs-window
                 (split-window old-emacs-window (glki-calc-new-window-size method old-emacs-window size wintype) (glki-vertical-split method))))
           (glki-fix-parent split new-pair-window)
           (setf (glki-opq-window-first-child new-pair-window) split)
           (setf (glki-opq-window-second-child new-pair-window) new-glk-window)
           (when (eq glk-root-window split)
             (setq glk-root-window new-pair-window))
           (cond ((or (memq 'glk-winmethod-above method) (memq 'glk-winmethod-left method))
                  (set-window-buffer new-emacs-window (glki-opq-window-buffer split))
                  (set-window-buffer old-emacs-window (glki-opq-window-buffer new-glk-window)))
                 ((or (memq 'glk-winmethod-below method) (memq 'glk-winmethod-right method))
                  (set-window-buffer new-emacs-window (glki-opq-window-buffer new-glk-window))))
           new-glk-window))))

(defun glk-window-close (win)
  (unless (eq win glk-root-window)
    (delete-window (glki-get-emacs-window win)))
  (glki-dispose-window win))

(defun glk-window-clear (win)
  (with-current-buffer (glki-opq-window-buffer win)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun glk-window-get-size (win)
  "Returns a list containing a glk window's width and height"
  (let ((emacs-window (glki-get-emacs-window win)))
    (list nil (window-width emacs-window) (window-body-height emacs-window))))

(defun glk-window-move-cursor (win xpos ypos)
  "In a Text Grid window sets the current cursor position"
  (let ((inhibit-read-only t))
    (when (eq 'glk-wintype-text-grid (glki-opq-window-type win))
      (with-current-buffer (glki-opq-window-buffer win)
        (forward-line (- (+ 1 ypos) (line-number-at-pos)))
        (while (< (line-number-at-pos) (+ 1 ypos))
          (insert "\n"))
        (move-to-column xpos t)))))

(defun glki-create-window-stream (window-type buffer stream-id)
  "Create a stream for this window. Use this stream id."
  (glki-opq-stream-create stream-id
                          0
                          buffer
                          (if (eq window-type 'glk-wintype-text-grid) 'glki-window-stream-text-grid 'glki-window-stream-text-buffer)
                          0
                          0
                          nil
                          nil
                          nil
                          nil))

(defun glk-set-window (win)
  (setq glk-current-stream (glki-opq-window-stream win))
  nil)

(defun glk-window-get-parent (win)
  (let ((windows glki-opq-window)
        result)
    (while (and windows (not result))
      (if (or (eq win (glki-opq-window-first-child (cdar windows)))
              (eq win (glki-opq-window-second-child (cdar windows))))
          (setq result (cdar windows))
        (setq windows (cdr windows))))
    result))

(defun glk-window-get-sibling (win)
  (let ((parent (glk-window-get-parent win)))
    (when parent
      (if (eq win (glki-opq-window-first-child parent))
          (glki-opq-window-second-child parent)
        (glki-opq-window-first-child parent)))))

(provide 'glk-window)
