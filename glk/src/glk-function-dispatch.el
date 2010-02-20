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

(eval-when-compile
  (require 'cl))

(require 'glk-base)

(defvar glk-process nil "Stores the process that is using the GLK library")

(defun glki-call-function (proc function-call)
  (glki-log function-call)
  (let ((form (car (read-from-string function-call))))
    (glki-send-return-value (eval form) (car form))))

(defun glki-glui32 (glui32)
  glui32)

(defun glki-char (char)
  (string-to-char char))

(defun glki-wintype (wintype)
  (case wintype
    (0 'glk-wintype-all-types)
    (1 'glk-wintype-pair)
    (2 'glk-wintype-blank)
    (3 'glk-wintype-text-buffer)
    (4 'glk-wintype-text-grid)
    (5 'glk-wintype-graphics)))

(defun glki-style (style)
  (case style
   (1 'glk-emphasizes-face)
   (3 'glk-header-face)
   (4 'glk-subheader-face)
   (t 'glk-normal-face)))

(defun glki-gestalt-selector (sel)
  (case sel
    (0 'glk-gestalt-version)))

(defconst glk-winmethod-position-decode
  '((3 glk-winmethod-below) (2 glk-winmethod-above) (1 glk-winmethod-right) (0 glk-winmethod-left)))

(defun glki-winmethod (winmethod)
  (list
   (cadr (assoc (logand winmethod #xf) glk-winmethod-position-decode))
   (if (>= winmethod #x20)
       'glk-winmethod-proportional
     'glk-winmethod-fixed)))

(defun glki-pointer (pointer)
  (cond
   ((symbolp pointer) pointer)
   ((equal pointer '(nil)) nil)))

(defun glki-string (string)
  string)

(defun glki-send-return-value (value function)
  "Format return values provided and send them to the calling process.
If value is the symbol glk-no-return then do not return to the calling process.
Uses glki-format-results to turn the value into a string."
  (when (and (not (eq value 'glk-no-return)) glk-process)
    (let ((formatted-values (glki-format-results function value)))
      (glki-log (concat "Return: \"" formatted-values "\""))
      (process-send-string glk-process formatted-values)))
  value)

(defconst glk-function-return-values
  (list (cons 'glk-window-open '(glki-format-pointer))
        (cons 'glk-select-waiting '(glki-format-event-type glki-format-pointer glki-format-glui32 glki-format-glui32
                                                           glki-format-pointer glki-format-string))
        (cons 'glk-select '(glki-format-event-type glki-format-pointer glki-format-glui32 glki-format-glui32
                                                           glki-format-pointer glki-format-string))
        (cons 'glk-char-to-lower '(glki-format-char))
        (cons 'glk-window-get-size '(glki-format-glui32 glki-format-glui32))
        (cons 'glk-cancel-line-event '(glki-format-event-type glki-format-pointer glki-format-glui32 glki-format-glui32
                                                           glki-format-pointer glki-format-string))
        (cons 'glk-get-buffer-stream '(glki-format-string glki-format-glui32)))
  "Lists of functions which are used to format each individual value in a list of return values into a string.")

(defun glki-format-char (char)
  (glki-format-int char))

(defun glki-format-glui32 (glui32)
  (glki-format-int glui32))

(defun glki-format-int (int)
  (format "%03d%d" (length (format "%d" int)) int))

(defun glki-format-pointer (pointer)
  (if pointer
      (glki-format-string (symbol-name pointer))
    (glki-format-string "NULL")))

(defun glki-format-string (string)
  (format "%03d%s" (length string) string))

(defun glki-format-event-type (event-type)
  (glki-format-int (glki-event-type->int event-type)))

(defun glki-event-type->int (event-type)
  (cond
   ((eq event-type 'glk-evtype-none) 0)
   ((eq event-type 'glk-evtype-lineinput) 3)))

(defun glki-void-formatter (value)
  "")

(defun glki-function-returns-value (function)
  "Returns true if this function returns a value, false otherwise."
  (assoc function glk-function-return-values))

(defun glki-get-formatters (function)
  "Returns a list of formatting functions for this function.
Returns the void formatter if the function returns no value."
  (if (glki-function-returns-value function)
      (cdr (glki-function-returns-value function))
    '(glki-void-formatter)))

(defun glki-format-results (function results)
  "Formats these results to send to the calling process.
If this function returns no value then the void formatter is used to create a string.
results may be a single value or a list of values which will be formatted and concatenated with newlines."
  (when (and (not (glki-function-returns-value function)) (null results)) (setq results 1))
  (unless (listp results) (setq results (list results)))
  (when (null results) (setq results '(nil)))
  (loop for formatter in (glki-get-formatters function)
        for result in results
        collecting (concat (funcall formatter result) "\n") into formatted-results
        finally return (apply 'concat formatted-results)))

(provide 'glk-function-dispatch)

