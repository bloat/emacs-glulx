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

;; (byte-recompile-directory "~/git/emacs-glulx/src" 0 t)
;; (byte-recompile-directory "~/git/emacs-glulx/glk/src" 0 t)

(add-to-list 'load-path (expand-file-name "~/git/emacs-glulx/src"))
(add-to-list 'load-path (expand-file-name "~/git/emacs-glulx/glk/src"))

(when (featurep 'glx-exec) (unload-feature 'glx-exec t))
(when (featurep 'glx-glk) (unload-feature 'glx-glk t))
(when (featurep 'glx-glulx) (unload-feature 'glx-glulx t))
(when (featurep 'glx-instructions) (unload-feature 'glx-instructions t))
(when (featurep 'glx-load) (unload-feature 'glx-load t))
(when (featurep 'glx-stack) (unload-feature 'glx-stack t))
(when (featurep 'glx-string) (unload-feature 'glx-string t))
(when (featurep 'glx-value) (unload-feature 'glx-value t))
(when (featurep 'glx-accelerated) (unload-feature 'glx-accelerated t))

(when (featurep 'glk-base) (unload-feature 'glk-base t))
(when (featurep 'glk-customize) (unload-feature 'glk-customize t))
(when (featurep 'glk) (unload-feature 'glk t))
(when (featurep 'glk-encoding) (unload-feature 'glk-encoding t))
(when (featurep 'glk-event) (unload-feature 'glk-event t))
(when (featurep 'glk-file) (unload-feature 'glk-file t))
(when (featurep 'glk-opaque) (unload-feature 'glk-opaque t))
(when (featurep 'glk-stream) (unload-feature 'glk-stream t))
(when (featurep 'glk-window) (unload-feature 'glk-window t))

(require 'glx-exec)
(require 'glx-glk)
(require 'glx-glulx)
(require 'glx-instructions)
(require 'glx-load)
(require 'glx-stack)
(require 'glx-string)
(require 'glx-value)
(require 'glx-accelerated)

(require 'glk-base)
(require 'glk-customize)
(require 'glk)
(require 'glk-encoding)
(require 'glk-event)
(require 'glk-file)
(require 'glk-opaque)
(require 'glk-stream)
(require 'glk-window)

(require 'ert)
(ert-delete-all-tests)

(load "glx-value-spec.el")
(load "glx-glulx-spec.el")
(load "glx-stack-spec.el")
(load "glx-string-spec.el")
(load "glx-exec-spec.el")
(load "glx-load-spec.el")
(load "glx-instructions-spec.el")
(load "glx-glk-spec.el")
(load "glx-compile-spec.el")
(load "glx-accelerated-spec.el")

(load "window-spec.el")
(load "stream-spec.el")
(load "event-spec.el")
(load "mode-spec.el")
(load "encoding-spec.el")
(load "file-spec.el")
(load "opaque-spec.el")
