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

(require 'glk-window)
(require 'glk-base)

(defvar glk-event-queue '())

(defvar glk-select-waiting nil)
(defvar glk-event-reentry-function #'identity)

(defun glki-add-event-to-queue (event)
  "Adds the given event to the event queue"
  (if glk-select-waiting
      (progn
        (setq glk-select-waiting nil)
        (glki-clear-line-event-request (glki-get-line-event-window event))
        (funcall glk-event-reentry-function event))
    (setq glk-event-queue (cons event glk-event-queue))))

(defun glki-get-next-event ()
  "Return the next event from the queue, and removes it from the queue"
  (let ((next-event (car (last glk-event-queue))))
    (setq glk-event-queue (butlast glk-event-queue))
    next-event))

;; Line Events

(defun glki-get-line-event-request (glk-window)
  "Returns any line event request on the given window"
  (get glk-window 'glk-line-event))

(defun glki-add-line-event-request (window-id c-buffer)
  "Mark the window has having a line event request on it"
  (put window-id 'glk-line-event c-buffer))

(defun glki-clear-line-event-request (window-id)
  "Clear any line event request on this window"
  (put window-id 'glk-line-event nil))

(defun glki-create-line-input-event (line-text c-buffer)
  "Creates a line input event object"
  (list 'glk-evtype-lineinput (glki-get-window-id (current-buffer)) (length line-text) 0 c-buffer line-text))

(defun glki-get-line-event-window (event)
  "Returns the window id for the event"
  (cadr event))

(defun glk-request-line-event (win buf maxlen initlen)
  (when (not (glki-get-line-event-request win))
    (glki-add-line-event-request win buf)
    (with-current-buffer (glki-opq-window-get-buffer win)
      (use-local-map glk-mode-map)))
  nil)

(defun glk-cancel-line-event (win)
  (if (glki-get-line-event-request win)
      (progn
        (with-current-buffer (glki-opq-window-get-buffer win)
          (glki-mode-add-input-to-event-queue))
        (glki-get-next-event))
    '(glk-evtype-none nil nil nil nil nil)))

(defun glki-get-c-buffer (event-request)
  event-request)

;; Char events

(defun glk-request-char-event (win)
  (when (not (glki-get-char-event-request win))
    (put win 'glk-char-event t)
    (with-current-buffer (glki-opq-window-get-buffer win)
      (use-local-map glk-char-input-mode-map))))

(defun glki-get-char-event-request (glk-window)
  (get glk-window 'glk-char-event))

(defun glki-create-char-input-event (char)
  (list 'glk-evtype-charinput (glki-get-window-id (current-buffer)) char 0))

(defun glki-clear-char-event-request (glk-window)
  (put glk-window 'glk-char-event nil))

(defun glk-cancel-char-event (win)
  (glki-clear-char-event-request win))

;; Select

(defun glk-select ()
  (let ((event (glki-get-next-event)))
    (if event
        (let ((event-window (glki-get-event-window event))
              (event-type (glki-get-event-type event)))
          (cond
           ((eq 'glk-evtype-lineinput event-type)
            (glki-clear-line-event-request event-window))
           ((eq 'glk-evtype-charinput event-type)
            (glki-clear-char-event-request event-window)))
          event)
      (setq glk-select-waiting t)
      'glk-no-return)))

(defun glki-get-event-type (event)
  (car event))

(defun glki-get-event-window (event)
  (cadr event))

(defun glki-mode-add-input-to-event-queue ()
  "Take the last line of input and add it as an event to the event queue."
  (interactive)
  (when (eq (point) (point-max))
    (let ((event-text
           (save-excursion
             (let ((start-of-input (previous-single-property-change (point-max) 'glk-text (current-buffer) (line-beginning-position)))
                   (end-of-input (point-max)))
               (if (= start-of-input (line-beginning-position))
                   ""
                 (buffer-substring-no-properties start-of-input end-of-input)))))
          (event-request (glki-get-line-event-request (glki-get-window-id (current-buffer)))))
      (newline)
      (when event-request
        (glki-add-event-to-queue
         (let ((new-event (glki-create-line-input-event event-text (glki-get-c-buffer event-request))))
           new-event))))))

(unless glk-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'glki-mode-add-input-to-event-queue)
    (setq glk-mode-map map)))

(defun glki-press-any-key ()
  (interactive)
  (when (glki-get-char-event-request (glki-get-window-id (current-buffer)))
    (glki-add-event-to-queue
     (glki-create-char-input-event last-command-event))))

(provide 'glk-event)
