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

;; All GLK Windows

(defopaque window first-child second-child buffer type is-key glk-line-event glk-char-event stream)

(defvar glk-root-window nil "Stores the GLK root window")

;; Creating, getting hold of, and deleting windows

(defun find-window (predicate)
  (some predicate glki-opq-window))

(defun glki-get-window-id (buffer)
  "Returns the glk window id for the given emacs buffer"
  (find-window #'(lambda (window) (if (eq buffer (glki-opq-window-get-buffer window)) window))))

(defun glki-kill-all-windows ()
  "Cleans up all glk windows and destroys the associated buffers"
  (mapcar #'glki-dispose-window glki-opq-window))

(defun glki-generate-new-window (type new-window-id new-stream-id rock)
  "Generate a new glk window"
  (let ((buffer (if (eq type 'glk-wintype-pair)
                    nil
                  (generate-new-buffer "*glk*"))))
    (if (eq 'glk-wintype-text-grid type)
      (with-current-buffer buffer
        (setq buffer-read-only t)))
    (let ((new-window (glki-opq-window-create new-window-id)))
      (glki-opq-window-set-buffer new-window buffer)
      (glki-opq-window-set-type new-window type)
      (glki-opq-window-set-stream new-window (glki-create-window-stream new-window-id new-stream-id))
      (glki-opq-window-set-rock new-window rock)
      (glki-set-glk-mode new-window-id)
      new-window)))

(defun glki-dispose-window (window)
  (if (glki-opq-window-get-stream window)
      (glki-opq-stream-dispose (glki-opq-window-get-stream window)))
  (if (glki-opq-window-get-buffer window)
      (kill-buffer (glki-opq-window-get-buffer window)))
  (glki-opq-window-dispose window))

(defun glki-create-first-window (type new-window-id new-stream-id rock)
  "Generate the first GLK window"
  (let ((window-on-glk-frame (frame-first-window glk-frame))
        (new-window (glki-generate-new-window type new-window-id new-stream-id rock)))
     (delete-other-windows window-on-glk-frame)
    (set-window-buffer window-on-glk-frame (glki-opq-window-get-buffer new-window))
    (setq glk-root-window new-window)))

;; Getting window properties

(defun glki-set-glk-mode (glk-window)
  "Sets glk-mode on the buffer for the given glk-window"
  (when (glki-opq-window-get-buffer glk-window)
    (with-current-buffer (glki-opq-window-get-buffer glk-window)
      (glk-mode))))

(defun glki-get-emacs-window (glk-window)
  "Returns the emacs windows displaying the given glk window"
  (get-buffer-window (glki-opq-window-get-buffer glk-window) glk-frame))

(defun glki-fix-parent (old-child new-child)
  (find-window #'(lambda (win) (cond ((eq (glki-opq-window-get-first-child win) old-child)
                                 (glki-opq-window-set-first-child win new-child))
                                ((eq (glki-opq-window-get-second-child win) old-child)
                                 (glki-opq-window-set-second-child win new-child))))))

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
         (condition-case the-error
             (let* ((new-glk-window (glki-generate-new-window wintype new-window-id new-window-stream-id rock))
                    (new-pair-window (glki-generate-new-window 'glk-wintype-pair new-parent-id new-parent-stream-id 0))
                    (old-emacs-window (glki-get-emacs-window split))
                    (new-emacs-window
                     (split-window old-emacs-window (glki-calc-new-window-size method old-emacs-window size wintype) (glki-vertical-split method))))
               (glki-fix-parent split new-pair-window)
               (glki-opq-window-set-first-child new-pair-window split)
               (glki-opq-window-set-second-child new-pair-window new-glk-window)
               (when (eq glk-root-window split)
                 (setq glk-root-window new-pair-window))
               (cond ((or (memq 'glk-winmethod-above method) (memq 'glk-winmethod-left method))
                      (set-window-buffer new-emacs-window (glki-opq-window-get-buffer split))
                      (set-window-buffer old-emacs-window (glki-opq-window-get-buffer new-glk-window)))
                     ((or (memq 'glk-winmethod-below method) (memq 'glk-winmethod-right method))
                      (set-window-buffer new-emacs-window (glki-opq-window-get-buffer new-glk-window))))
               new-glk-window)
             (error nil)))))

(defun glk-window-close (win)
  (unless (eq win glk-root-window)
    (delete-window (glki-get-emacs-window win)))
  (glki-dispose-window win))

(defun glk-window-clear (win)
  (save-current-buffer
    (set-buffer (glki-opq-window-get-buffer win))
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun glk-window-get-size (win)
  "Returns a list containing a glk window's width and height"
  (let ((emacs-window (glki-get-emacs-window win)))
    (list nil (window-width emacs-window) (window-body-height emacs-window))))

(defun glk-window-move-cursor (win xpos ypos)
  "In a Text Grid window sets the current cursor position"
  (let ((inhibit-read-only t))
    (when (eq 'glk-wintype-text-grid (glki-opq-window-get-type win))
      (with-current-buffer (glki-opq-window-get-buffer win)
        (forward-line (- (+ 1 ypos) (line-number-at-pos)))
        (while (< (line-number-at-pos) (+ 1 ypos))
          (insert "\n"))
        (move-to-column xpos t)))))

(defun glki-create-window-stream (glk-window stream-id)
  "Create a stream for this window. Use this stream id."
  (let ((stream (glki-opq-stream-create stream-id)))
    (glki-opq-stream-set-buffer stream (glki-opq-window-get-buffer glk-window))
    (glki-opq-stream-set-type stream 'glki-window-stream)
    stream))

(defun glk-set-window (win)
  (setq glk-current-stream (glki-opq-window-get-stream win))
  nil)

(provide 'glk-window)
