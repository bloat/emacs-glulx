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

(add-to-list 'load-path (expand-file-name "~/git/emacs-glx/src"))
(add-to-list 'load-path (expand-file-name "~/git/emacs-glx/glk/src"))

(require 'glx-exec)
(require 'glx-glk)
(require 'glx-glulx)
(require 'glx-instructions)
(require 'glx-load)
(require 'glx-stack)
(require 'glx-string)
(require 'glx-value)
(require 'glx-compile)

(require 'glk-base)
(require 'glk-customize)
(require 'glk)
(require 'glk-encoding)
(require 'glk-event)
(require 'glk-file)
(require 'glk-function-dispatch)
(require 'glk-opaque)
(require 'glk-stream)
(require 'glk-window)

(load "glx-value-spec.el")
(load "glx-glulx-spec.el")
(load "glx-stack-spec.el")
(load "glx-string-spec.el")
(load "glx-exec-spec.el")
(load "glx-load-spec.el")
(load "glx-instructions-spec.el")
(load "glx-glk-spec.el")
(load "glx-compile-spec.el")

(load "window-spec")
(load "function-dispatch-spec")
(load "stream-spec")
(load "event-spec")
(load "mode-spec")
(load "event-spec")
(load "encoding-spec")
(load "file-spec")
(load "opaque-spec")
