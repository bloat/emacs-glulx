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

(add-to-list 'load-path (expand-file-name "~/git/emacs-glulx/src"))
(add-to-list 'load-path (expand-file-name "~/git/emacs-glulx/glk/src"))

(unload-feature 'glx-exec t)
(unload-feature 'glx-glk t)
(unload-feature 'glx-glulx t)
(unload-feature 'glx-instructions t)
(unload-feature 'glx-load t)
(unload-feature 'glx-stack t)
(unload-feature 'glx-string t)
(unload-feature 'glx-value t)
(unload-feature 'glx-compile t)

(unload-feature 'glk-base t)
(unload-feature 'glk-customize t)
(unload-feature 'glk t)
(unload-feature 'glk-encoding t)
(unload-feature 'glk-event t)
(unload-feature 'glk-file t)
; (unload-feature 'glk-function-dispatch t)
(unload-feature 'glk-opaque t)
(unload-feature 'glk-stream t)
(unload-feature 'glk-window t)

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
; (require 'glk-function-dispatch)
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

(load "window-spec.el")
;; (load "function-dispatch-spec")
(load "stream-spec.el")
(load "event-spec.el")
(load "mode-spec.el")
(load "encoding-spec.el")
(load "file-spec.el")
(load "opaque-spec.el")
