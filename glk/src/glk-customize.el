(defgroup glk nil "GLK" :group 'games)

(defface glk-normal-face '((default ())) "Face used to display normal Glk text" :group 'glk)
(defface glk-emphasized-face '((default (:weight bold))) "Face used to display emphasized Glk text" :group 'glk)
(defface glk-header-face '((default (:weight ultra-bold :underline t))) "Face used to display Glk headers" :group 'glk)
(defface glk-subheader-face '((default (:underline t))) "Face used to display Glk subheaders" :group 'glk)

(provide 'glk-customize)
