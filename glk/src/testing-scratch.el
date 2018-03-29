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

(glki-init (make-frame (list (cons 'height 61) (cons 'width 60))))

(glk-window-open nil nil 0 'glk-wintype-text-buffer 0 'window1 nil)

(glk-window-open 'window1
                 (list 'glk-winmethod-above 'glk-winmethod-proportional)
                 50 'glk-wintype-text-buffer 0 'window2 'pair)


(glk-window-open 'window2
                 (list 'glk-winmethod-above 'glk-winmethod-proportional)
                 50 'glk-wintype-text-buffer 0 'window3 'pair2)


glk-root-window


(mapcar #'glki-clear-glk-plist-symbols '(window1 pair window2 window3 pair2))

(glki-get-first-child glk-root-window)
(glki-get-second-child glk-root-window)
(glki-get-buffer 'window1)

(glki-end)

(put 'bosh 'nish 'has)

