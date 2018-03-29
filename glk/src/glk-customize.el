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

(defgroup glk nil "GLK" :group 'games)

(defface glk-normal-face '((default ())) "Face used to display normal Glk text" :group 'glk)
(defface glk-emphasized-face '((default (:weight bold))) "Face used to display emphasized Glk text" :group 'glk)
(defface glk-header-face '((default (:weight ultra-bold :underline t))) "Face used to display Glk headers" :group 'glk)
(defface glk-subheader-face '((default (:underline t))) "Face used to display Glk subheaders" :group 'glk)

(provide 'glk-customize)
